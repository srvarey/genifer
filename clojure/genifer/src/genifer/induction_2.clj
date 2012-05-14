;;; ***** Inductive learner #2 (top-down)
;;; ==========================================================
;;;; An implementation of MINI-HYPER and HYPER from Ivan Bratko's 2001 book
;;;; "Prolog Programming for Artificial Intelligence" 3rd ed, ch.19

;;; Some historical top-down learners:
;;; 1. FOIL
;;; 2. FOCL
;;; 3. MIS
;;; 4. Progol
;;; 5. Tracy
;;; 6. HYPER

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
(declare induce)

;;; Each hypothesis has a list of clauses
;;; Each clause is either a rule or a fact (ie, with empty premise (= body))
; hypothesis = [ score = 0, clauses = nil ]

;;; "args" contains the variables that appear in the clause;  this is for programming convenience
; clause = [ head, body, args ]

(def *max-clause-length* 4)

(def clausendex 0)
(def *max-clausendex* 0)
(def argndex 0)
(def *max-argndex* 0)
(def literalndex 0)

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

(def background-literals '(
	[(parent ?1 ?2),	(?1 ?2)]
	[(male   ?1),		(?1)]
	[(female ?1),		(?1)]
	))

(def max-literalndex 3)

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

;;; Note:  due to the use of similar algorithmic ideas (eg best-first search), the procedures
;;; of this module will have the suffix '-I' to distinguish it from deduction.

;;; Best-first search algorithm:
;;; 1. If priority queue is empty, return fail
;;; 2. Remove first item from priority list and test it
;;; 3. Refine the current hypothesis
;;; 4. Merge results with priority queue
(defn induce []
	(setf priority-list (list starting-hypothesis))
	(loop
		;; 1. If priority queue is empty, return fail
		(if (null priority-list)
			(return 'fail))
		;; 2. Remove first item from priority list and test it
		;(print-priority-list)
		(setf best (car priority-list))
		(setf priority-list (cdr priority-list))
		;; Test it
		(if (and (check-complete   best)
				 (check-consistent best))
			(return best))
		;(break "****************** tested 1 hype ********************")
		;; 3. Refine the current hypothesis
		(setf refinements (refine-hyp best))
		;; 4. Merge results with the priority queue
		(****DEBUG 2 "merging with P-queue")
		(sort refinements #'compare-scores)
		(setf priority-list (merge 'list refinements priority-list #'compare-scores))))

;;; Compare the scores of 2 priority-list items
(defn compare-scores [new old]
  (< (score new) (score old)))

;;; Find refinements of a hypothesis
;;; Return:  a list of refinements
; There are 3 possible ways to generate a new hype:
; 1)	equating 2 distinct variables in the old hype
; 2)	adding a new background literal to clause (with new vars as arguments)
; 3)	refine a variable into a structured term, perhaps X ==> father of X?
(defn refine-hype [hype]
	;; Initialize
	(setf 	results		(list nil)
			clausendex	0)
	;; Select a clause from the hypothesis
	(for [clause (clauses hype)]
		;; Try to make variable substitutions
		(****DEBUG 2 "Trying variable subs")
		(setf args (args clause))
		(setf rest-args args)
		(for [pivot1 args]
			(setf rest-args (cdr rest-args))
			(for [pivot2 rest-args]
				(if (not (= pivot1 pivot2))
					(do
						;; Make a copy
						(setf hyp1 (make-copy hype))
						;; Find the current clause in copy
						(setf clause1 (nth clausendex (clauses hyp1)))
						;; Make the substitution
						(setf sub (list (cons pivot2 pivot1)))
						(setf (head clause1) (do-subst (head clause1) sub))
						(setf (body clause1) (do-subst (body clause1) sub))
						;; Delete the variable from the clause's variable list
						(setf (args clause1) (delete pivot2 (args clause1)))
						;; Output as refined hypothesis:
						;; Calculate new score
						(setf (score hyp1) (- (score hyp1) 1))
						(nconc results (list hyp1))))))
		;; Try to add literals
		(****DEBUG 2 "Trying add literals")
		(for [literal background-literals]
			;; Make a copy
			(setf hyp1 (make-copy hype))
			;; Find the current clause in copy
			(setf clause1 (nth clausendex (clauses hyp1)))
			;; 'Standardize apart'
			(setf subs (standardize-apart clause1))
			(setf (head clause1) (do-subst (head clause1) subs))
			(setf (body clause1) (do-subst (body clause1) subs))
			(setf (args clause1) (do-subst (args clause1) subs))
			;; Add literal to hypothesis
			(if (null (body clause))
				(setf   (body clause1) (list 'ID    (car literal)))
				(if (eql 'ID (car (body clause)))
					(setf (body clause1) (list 'Z-AND (second (body clause1)) (car literal)))
					(setf (body clause1) (list 'Z-AND (body clause1)          (car literal)))))
			(nconc (args clause1) (cdr literal))
			;; Output as refined hypothesis:
			;; Calculate new score
			(setf (score hyp1) (+ 10
						(score hyp1)
						(length (cdr literal))))
			(nconc results (list hyp1)))
		(incf clausendex))
	;; Return the list -- first item is nil so it's discarded:
	(cdr results)))

;;; 'Standardizing apart' -- seems to be completely wacky -- need new version
(defn standardize-apart [clause]
  (setf *new-vars* nil)
  (find-all-vars (head clause))
  (find-all-vars (body clause))
  (create-unique-subs *new-vars*))

; why is this needed?
(defn make-copy [hype]
  (setf clause-list (list nil))
  (dolist (clause (clauses hype))
    (nconc clause-list (list (makenstance 'clause
                               :head (copy-tree (head clause))
                               :body (copy-tree (body clause))
                               :args (copy-tree (args clause))))))
  (makenstance 'hypothesis
    :score (score hype)
    :clauses (cdr clause-list)))

(def *completeness-tolerance* 0)	; how many failures to tolerate

;;; ***** Check that hype covers all positive examples
; algorithm:
; 1.	add clauses in hype to Working Memory
; 2.	for each positive example e+
; 3.		call backward chaining on e+
; 4.	count total number of failures
; 5.	restore Working Memory to prior state
; -- Note:  function can return earlier by short-circuit, so we use lazy "take n" and "filter"
;; -- To-do:	may set a limit for testing hypes
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

(def *consistency-tolerance* 0)		; how many failures to tolerate

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