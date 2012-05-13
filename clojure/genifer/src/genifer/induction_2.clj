;;; ***** Inductive learner #2 (top-down)
;;; ==========================================================

;;; Some historical top-down learners:
;;; 1. FOIL
;;; 2. FOCL
;;; 3. MIS
;;; 4. Progol
;;; 5. Tracy
;;; 6. HYPER

;;;; **** Inductive learner #2 -- "Hyper"
;;;; An implementation of MINI-HYPER and HYPER from Ivan Bratko's 2001 book
;;;; "Prolog Programming for Artificial Intelligence" 3rd ed, ch.19

;;; ************************************* TO-DO *****************************************

;;; Examples should be stored as facts in "main memory".  For induction, we ask the prover if
;;; a hypothesis covers the example or not, ie query the example.  So it is as if the examples
;;; do not exist in the KB, ie their TVs can be set to 'fail.

; so perhaps we need a special func to explain a known fact?  which is a different operation than
; backward chaining?  Or we simply discard the fact that is being queried.

(ns genifer.induction_2)

(declare induce)

;;; Each hypothesis has a list of clauses
;;; Each clause is either a rule or a fact (ie, with empty premise (= body))
(defclass hypothesis () (
  (score :initarg :score :accessor score :initform 0)
  (clauses :initarg :clauses :accessor clauses :initform nil)
))

;;; "args" contains the variables that appear in the clause;  this is for programming convenience
(defclass clause () (
  (head :initarg :head :accessor head)
  (body :initarg :body :accessor body)
  (args :initarg :args :accessor args)
))

(def *max-clause-length* 4)

(def clausendex 0)
(def *max-clausendex* 0)
(def argndex 0)
(def *max-argndex* 0)
(def literalndex 0)

(def *starting-hypothesis*
  (makenstance 'hypothesis
    :score 0
    :clauses (list (makenstance 'clause
                     :head '(has-dau ?1)
                     :body nil
                     :args '(?1))))

;  (makenstance 'hypothesis
;    :score 0
;    :clauses (list (makenstance 'clause
;                     :head '(has-dau ?1)
;                     :body '(Z-AND (parent ?1 ?2) (female ?2))
;                     :args '(?1 ?2))))
)

(def *background-literals* (list
(cons '(parent ?1 ?2)     '(?1 ?2))
(cons '(male   ?1)        '(?1))
(cons '(female ?1)        '(?1))
))

(def max-literalndex 3)

; Each clause is ((a list of literals) . (a list of variables in the clause))

;;; Please refer to example 6 for the background knowledge in "memory.lisp"

;;; Positive examples
(def *examples+* (list
   '(has-dau tom)
   '(has-dau bob)
   '(has-dau pat)
;   '(long-hair mary)
;   '(long-hair jane)
))

;;; Negative examples
(def *examples-* (list
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
  (setf priority-list (list *starting-hypothesis*))
  (loop
    ;; 1. If priority queue is empty, return fail
    (if (null priority-list)
      (return 'fail))
    ;; 2. Remove first item from priority list and test it
    ;(print-priority-list)
    (setf best (car priority-list))
    (setf priority-list (cdr priority-list))
    ;; Test it
    (if (and (prove-complete   best)
             (prove-consistent best))
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
(defn refine-hyp [hyp]
  ;; Initialize
  (setf results       (list nil)
        clausendex  0)
  ;; Select a clause from the hypothesis
  (dolist (clause (clauses hyp))
    ;; Try to make variable substitutions
    (****DEBUG 2 "Trying variable subs")
    (setf args (args clause))
    (setf rest-args args)
    (dolist (pivot1 args)
      (setf rest-args (cdr rest-args))
      (dolist (pivot2 rest-args)
        (if (not (equal pivot1 pivot2))
          (progn
            ;; Make a copy
            (setf hyp1 (make-copy hyp))
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
    (dolist (literal *background-literals*)
      ;; Make a copy
      (setf hyp1 (make-copy hyp))
      ;; Find the current clause in copy
      (setf clause1 (nth clausendex (clauses hyp1)))
      ;; 'Standardize apart'
      (setf subs (standardize-apart clause1))
      (setf (head clause1) (do-subst (head clause1) subs))
      (setf (body clause1) (do-subst (body clause1) subs))
      (setf (args clause1) (do-subst (args clause1) subs))
      ;(****BR "huh")
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
  (cdr results))

;;; 'Standardizing apart' -- see unification.lisp for details
(defn standardize-apart [clause]
  (setf *new-vars* nil)
  (find-all-vars (head clause))
  (find-all-vars (body clause))
  (create-unique-subs *new-vars*))

(defn make-copy [hyp]
  (setf clause-list (list nil))
  (dolist (clause (clauses hyp))
    (nconc clause-list (list (makenstance 'clause
                               :head (copy-tree (head clause))
                               :body (copy-tree (body clause))
                               :args (copy-tree (args clause))))))
  (makenstance 'hypothesis
    :score (score hyp)
    :clauses (cdr clause-list)))

;;; Prove that hyp covers all positive examples
(defn prove-complete [hyp]
  (setf memtems-list (list nil))
  (dolist (clause (clauses hyp))
    (****DEBUG 2 "[~a] prove-complete: <~a> ~a <- ~a" ct (score hyp) (head clause) (body clause))
    (setf memtem (add-rule-to-mem (head clause) (body clause)))
    (nconc memtems-list (list memtem)))
  (incf ct)
  (if (eql ct 0)  ;94
    (break))
  ;; For all positive examples...
  (dolist (e *examples+*)
    (backward-chain e)
    ;; Proof failed?
    (if (or (null (solutions proof-tree))
            (equal 'fail (solutions proof-tree)))
      ;; Return failure
      (progn
        (****DEBUG 2 "COMPLETENESS        ........failed: ~a" e)
        (setf (score hyp) (+ (score hyp) 30))
        (dolist (item (cdr memtems-list))
          (delete-memorytem item))
        (return-from prove-complete nil))))
  (dolist (item (cdr memtems-list))
    (delete-memorytem item))
  (****DEBUG 2 "COMPLETENESS        ........succeed")
  t)

(def ct 0)

;;; Prove that hyp does NOT cover any negative example
(defn prove-consistent [hyp]
  (setf memtems-list (list nil))
  (dolist (clause (clauses hyp))
    (****DEBUG 2 "prove-consistent: <~a> ~a <- ~a" (score hyp) (head clause) (body clause))
    (setf memtem (add-rule-to-mem (head clause) (body clause)))
    (nconc memtems-list (list memtem)))
  ;; For all negative examples...
  (dolist (e *examples-*)
    (backward-chain e)
    ;; Proof succeeded?
    (if (and (not (equal 'fail (solutions proof-tree)))
             (not (null (solutions proof-tree))))
      ;; Return failure
      (progn
        (****DEBUG 2 "CONSISTENCY        ........failed: ~a" e)
        (dolist (item (cdr memtems-list))
          (delete-memorytem item))
        (return-from prove-consistent nil))))
  (dolist (item (cdr memtems-list))
    (delete-memorytem item))
  (****DEBUG 2 "CONSISTENCY        ........succeed")
  t)

(defun print-priority-list []
  (setf ptr priority-list)
  (loop
    (****DEBUG 2 "<~a>" (score (car ptr)))
    (dolist (clause (clauses (car ptr)))
      (****DEBUG 2 "------ ~a <- ~a [~a]" (head clause) (body clause) (args clause)))
    (setf ptr (cdr ptr))
    (if (null ptr) (return)))
  (****DEBUG 2 "~%"))

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