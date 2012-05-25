;;; ***** Inductive learner #2 (top-down)
;;; ==========================================================
;;;; An implementation of MINI-HYPER and HYPER from Ivan Bratko's 2001 book
;;;; "Prolog Programming for Artificial Intelligence" 3rd ed, ch.19

;; Some historical top-down learners:
;; 1. FOIL
;; 2. FOCL
;; 3. MIS
;; 4. Progol
;; 5. Tracy
;; 6. HYPER

;; The most expensive step is the refinement of hypothesis that changes a variable into a new structural term, because there are a huge number of constants that can be composed with the old variable.
;; Possible optimizations:
;;	1. create only terms that "make sense" -- call composition semantics
;;	2. use "idioms", ie, compositions that appear frequently
;;	3. use atoms or compositions that are currently under attention in Working Memory
;;	4. make use of the "type" of X, for example if X "is-a" man, etc

;;; ************************************* TO-DO *****************************************
;;; Examples should be stored as facts in "main memory".  For induction, we ask the prover if
;;; a hypothesis covers the example or not, ie query the example.  So it is as if the examples
;;; do not exist in the KB, ie their TVs can be set to 'fail.

; so perhaps we need a special func to explain a known fact?  which is a different operation than
; backward chaining?  Or we simply discard the fact that is being queried.

(ns genifer.induction_2
	(:require [genifer.backward_chaining		:as backward])
	(:require [genifer.knowledge_representation	:as knowledge])
)
(def executor2 (Executors/newCachedThreadPool))
(declare induce refine check-complete check-consistent)

;;; Each hypothesis has a list of clauses
;;; Each clause is either a rule or a fact (ie, with empty premise (= body))
; hypothesis = [ score = 0, clauses = nil ]

;;; "args" contains the variables that appear in the clause;  this is for programming convenience
; clause = [ head, body, args ]

(def *max-clause-length* 4)
(def *max-literalndex* 3)

(def starting-hypothesis
	[0 ; score
	; clauses:
    (list							; clauses
		; 1 clause = [ head, body, args ]
		['(has-dau X), nil, '(X)]
))

;  (makenstance 'hypothesis
;    :score 0
;    :clauses (list (makenstance 'clause
;                     :head '(has-dau ?1)
;                     :body '(Z-AND (parent ?1 ?2) (female ?2))
;                     :args '(?1 ?2))))

(def *background-literals* '(
	(parent X Y)
	(male   X)
	(female X)
	))

; Each clause is [ (a list of literals) , (a list of variables in the clause) ]

;;; Please refer to example 6 for the background knowledge in "memory.lisp" (old)

;;; Positive examples
(def examples+ (list
   '(has-dau tom)
   '(has-dau bob)
   '(has-dau pat)
;   '(long-hair mary)
;   '(long-hair jane)
))

;;; Negative examples
(def examples- (list
   '(has-dau pam)
   '(has-dau jim)
;   '(long-hair john)
;   '(long-hair pete)
;   '(long-hair rita)
))

;;; Best-first search algorithm:
;;; 1. If priority queue is empty, return fail
;;; 2. Remove first item from priority list and test it
;;; 3. Refine the current hypothesis
;;; 4. Merge results with priority queue
(defn induce [hype]
	;; get initial hype
	;; test initial hype
	;; if success return
	;; get a list of refinements
	;; recurse on all those hypes
	(if (> (+ (check-complete	hype)
			  (check-consistent	hype)) 1.5)
		; accept hype
		(let [refinements (refine hype)]
			;; Spawn processes -- no dependencies
			;; It may be desirable to sort the candidates and try the promising ones first
			(doseq [refined refinements]
				(induce refined)
))))

;;; Find refinements of a hypothesis
;; INPUT:		hype
;; OUTPUT:	a list of refinements
; There are 3 possible ways to generate a new hype:
; 1)	equating 2 distinct variables in the old hype
; 2)	adding a new background literal to clause (with new vars as arguments)
; 3)	refine a variable into a structured term, eg: X ==> a X b
(defn refine [hype]
	(concat
		(refine-by-var	hype)
		(refine-clause	hype)
		(refine-term	hype)))

;; Refine by equating 2 distinct variables
;; -- may need to remove redundant refinements
(defn refine-by-var [hype]
	(for [	;; For each clause in hype
			clause	hype
			;; For all distinct pairs of variables in hype
			[x y]	(combinatorics/combinations (vars-of hype) 2) ]
		;; Create the substitution { X/Y } and apply it to clause
		(map #(subst/substitute-atomic [x y] &) clause)))

;; Find all variables in hype
(defn vars-of [hype]
	(memoize vars-of_))		; memoization for better performance

(defn vars-of_ [hype]
	(apply concat
		(for [clause hype]
			(concat
				(filter variable? (first  clause))
				(filter variable? (second clause))))))

;; Refine by adding a literal to a clause
(defn refine-clause [hype]
	(for [	;; For each clause in hype
			clause	hype
			;; For each "addable" background literal
			literal	*background-literals* ]
		(assoc-in hype
			(unify/standardize-apart literal))))

;; Refine a term "structurally"
;; -- change a variable X into aX, Xa, aXb, ..., where ax, xa, axb, ... are common constants, and such that these constants are compatible with X's type
;; The variable X is already in some kind of context, so the refinement of X should be congruent with it.
;; If X occurs as the only variable in a term T, then T is defining of X, which may lead to other truths about X.
;; Problem of joint refinement of aX etc.
(defn refine-by-term [hype]
	()
)

(def *completeness-tolerance* 0)	; how many failures to tolerate
(def *consistency-tolerance* 0)		;		''

;;; ***** Check that hype covers all positive examples
; algorithm:
; 1.	add clauses in hype to Working Memory
; 2.	for each positive example e+
; 3.		call backward chaining on e+
; 4.	count total number of failures
; 5.	restore Working Memory to prior state
; -- Note:  function can return earlier by short-circuit, so we use lazy "take n" and "filter"
;; -- To-do:	may set a limit for testing hypes
;; OUTPUT: a score in [0,1] = passed so far / tested so far
(defn check-complete [hype]
	;; Keep a reference to the old Working Memory, so we can restore to this state afterwards
	(let [old-WM @knowledge/working-mem]
		;; Add all clauses in hype to Working Memory
		(doseq [clause hype]
			(add-to-WM clause))
		;; For each positive example e+ :
		(let [score (take *completeness-tolerance* (filter false?
			(for [e+ examples+]
				(backward/solve-goal e+))))]
			;; Restore prior state of Working Memory
			(send knowledge/working-mem (fn [_] old-WM))
			score)))

;;; Check that hype does NOT cover any negative example -- same as check-complete
(defn check-consistent [hype]
	(let [old-WM @knowledge/working-mem]
		(doseq [clause hype]
			(add-to-WM clause))
		;; For each negative example e- :
		(let [score (take *consistency-tolerance* (filter #(not (false? &))
			(for [e- examples-]
				(backward/solve-goal e-))))]
			(send knowledge/working-mem (fn [_] old-WM))
			score)))

; (defun print-priority-list []
  ; (setf ptr priority-list)
  ; (loop
    ; (****DEBUG 2 "<~a>" (score (car ptr)))
    ; (dolist (clause (clauses (car ptr)))
      ; (****DEBUG 2 "------ ~a <- ~a [~a]" (head clause) (body clause) (args clause)))
    ; (setf ptr (cdr ptr))
    ; (if (null ptr) (return)))
  ; (****DEBUG 2 "~%"))

; (defun get-clause-length (clause)
;   (setf length 1
;         ptr    (second clause))
;   (loop
;     (if (equal (car ptr) 'Z-AND)
;       (incf length)
;       (return))
;     (setf ptr (cadr ptr)))
;   (****DEBUG "clause length = ~a" length)
;   length
; )