;;;; =======================================================================
;;;;    Deduction: backward-chaining, using pure Z logic, best-first search
;;;; =======================================================================

;;;; Genifer/deduction.lisp
;;;;
;;;; Copyright (C) 2009 Genint
;;;; All Rights Reserved
;;;;
;;;; Written by YKY
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU Affero General Public License v3 as
;;;; published by the Free Software Foundation and including the exceptions
;;;; at http://opencog.org/wiki/Licenses
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License
;;;; along with this program; if not, write to:
;;;; Free Software Foundation, Inc.,
;;;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
;;;; ------------------------------------------------------------------------------
(defparameter *max-depth*      7)     ; maximum search depth
(defparameter *max-rule-nodes* 150)   ; maximum number of rule nodes in proof tree
(defparameter max-rule-length  20)    ; rule length = \# of arguments (literals) it has
(defparameter W0 50.0)                ; constant used in calculating confidence from w
(defparameter abduction-penalty 0.2)  ; < 1.0;  used to reduce the scores of abductive steps
(defparameter facts-boost       1.5)  ; > 1.0;  increases the scores of facts
(defparameter args-decay        0.8)  ; < 1.0;  decreases the scores of consecutive args in a rule
(defparameter testing-decay     0.98) ; < 1.0;  decreases scores of successive literal tests
(defclass tree-node () (
  (parent     :initarg :parent     :accessor parent                    :type symbol)
  (solutions  :initarg :solutions  :accessor solutions  :initform nil  :type (list solution))
  (node-data  :initarg :node-data  :accessor node-data  :initform nil  :type list)
  (node-type  :initarg :node-type  :accessor node-type  :initform #\X  :type char)
))

;;; Each solution is a pair:  (sub, message)
(defclass solution () (
  (sub     :initarg :sub     :accessor sub     :initform nil  :type list)
  (message :initarg :message :accessor message :initform nil)
))

;;; Class for a rule data item within a proof tree node
;;; rule-type is either #\H or #\B depending if the top node is a head literal or body literal
(defclass rule () (
; (op         :initarg :op         :accessor op         :type single-float)
  (literals   :initarg :literals   :accessor literals   :type list           :initform nil)
  (parameters :initarg :parameters :accessor parameters :type (list single-float))
  (rule-type  :initarg :rule-type  :accessor rule-type  :type char           :initform #\X)
  (confidence :initarg :confidence :accessor confidence :type single-float)
  (factor     :initarg :factor     :accessor factor     :type single-float)
))

;;; An item of the priority list.
;;;     list-data = either a sub-goal or a fact/rule
;;;     ptr       = pointer to proof-tree node
;;;     data-type = rule-type, see above
(defclass p-list-item () (
  (score     :initarg :score      :accessor score      :type single-float  :initform 0.0)
  (depth     :initarg :depth      :accessor depth      :type fixnum        :initform 0)
  (list-data :initarg :list-data  :accessor list-data  :type symbol)
  (data-type :initarg :data-type  :accessor data-type  :type char          :initform #\X)
  (ptr       :initarg :ptr        :accessor ptr        :type symbol)
))

(defvar proof-tree      nil)         ; The initial proof-tree
(defvar priority-list   nil)
(defvar new-states-list nil)
(defvar *explanation*   nil)         ; the result of abduction
(defvar *abduct-mode*   nil)         ; true if abduction mode is ON

(defvar timer             0)
(defvar *goal-nodes*      0)
(defvar *rule-nodes*      0)
(defvar *fact-nodes*      0)

(setf *print-circle* t)               ; Lisp key that allows printing of circular objects
(defun backward-chain (query)
  ;; 0. Initialize
  (setf proof-tree (make-instance 'tree-node :parent nil))
  (setf *goal-nodes*  1
        *rule-nodes*  0
        *fact-nodes*  0)
  ;; On entry, priority list contains the goal query
  (setf priority-list (list (make-instance 'p-list-item
                              :score      0.0
                              :depth      0
                              :list-data  query
                              :ptr        proof-tree)))
  (loop
    ;; Has found result?
    (if (or (not (null (solutions proof-tree)))
            (equal 'fail (solutions proof-tree)))
      (return))
    ;(if (> (- (get-internal-run-time) timer) 10000000)
    ;   (return))
    ;; 1. Remove first item from priority list
    (if (null priority-list)
      (return))
    (setf best (car priority-list))               ; "best" = top item on priority list
    (setf priority-list (cdr priority-list))
    ;; The current tree node being processed:
    (setf node (ptr best))
    ;; Maximum search depth reached?
    (setf depth (depth best))
    (if (> depth *max-depth*)
      (progn (****DEBUG 3 "backward-chain: FAIL -- past max tree depth")
        (return 'fail)))
    (if (> *rule-nodes* *max-rule-nodes*)
      (progn (****DEBUG 3 "backward-chain: FAIL -- past max num of rules")
        (return 'fail)))
    (if (> *goal-nodes* *max-rule-nodes*)
      (progn (****DEBUG 3 "backward-chain: FAIL -- past max num of goals")
        (return 'fail)))
    (****DEBUG 1 "~%")
    (****DEBUG 1 "backward-chain: proof-tree node = ~a" (node-data node))
    ;; 2. Process the current node, which can be a sub-goal, a rule, or a fact:
    (setf new-states-list (list nil))
    ;; Is it a sub-goal?
    (if (listp (list-data best))
      (process-subgoal node best)
      ;; Else... it is either a fact/rule
      (let ((formula (list-data best)))
        ;; Is it a fact?
        (if (null (body formula))
          (process-fact node formula best)
          ;; It's a rule:
          (process-rule node formula best))))
    ;; 3. Merge results with priority queue
    ;;    Results are stored in "new-states-list"
    (setf new-states-list (cdr new-states-list))   ; remove first element which is a dummy nil
    (sort new-states-list #'compare-scores)
    (****DEBUG 1 "backward-chain: new-states-list has ~a element(s)" (length new-states-list))
    (setf priority-list (merge 'list new-states-list priority-list #'compare-scores))
    (print-priority-list)
    (print-proof-tree)))

;;; compare the scores of 2 priority-list items
(defun compare-scores (new old)
  (> (score new) (score old)))
(defun process-subgoal (node best)
  (setf best-data (list-data best))
  (****DEBUG 1 "process-subgoal: sub-goal ~a" best-data)
  ;; store the sub-goal in the proof tree node
  (setf (node-data node) (list best-data))
  (setf (node-type node) (data-type best))
  ;; Fetch rules that match the subgoal:
  ;; 'fetch-rules' is defined in memory.lisp
  (multiple-value-bind (facts-list rules-list) (fetch (car best-data))
    (dolist (formula facts-list)
      ;; standardize apart head-of-subgoal and formula-to-be-added
      (setf subs (standardize-apart (head formula) nil)
                 (head formula) (do-subst (head formula) subs)
                 (body formula) nil)
      (****DEBUG 1 "process-subgoal: adding fact... ~a" (head formula))
      ;; create proof tree node for the fact, with initial tv = nil
      (setf new-node (make-instance 'tree-node :parent node))
      (incf *fact-nodes*)
      ;; add node as current node's child
      (nconc (node-data node) (list new-node))
      ;; prepare priority list item:
      ;; score := confidence * (1 - partial_length / max_length)
      (setf depth (depth best))
      (setf score (* facts-boost (confidence formula) (- 1.0 (/ depth *max-depth*))))
      (setf new-state (make-instance 'p-list-item
                        :score     score
                        :depth     (+ 1 depth)
                        :list-data formula
                        :ptr       new-node))
      (nconc new-states-list (list new-state)))
    ;; Do the same for rules:
    (dolist (formula rules-list)
      ;; standardize apart head-of-subgoal and formula-to-be-added
      (setf subs (standardize-apart (head formula) (body formula))
                 (head formula) (do-subst (head formula) subs)
                 (body formula) (do-subst (body formula) subs))
      (setf body (body formula))
      (****DEBUG 1 "process-subgoal: adding rule... ~a <- ~a" (head formula) body)
      ;; create proof tree node and store rule-formula in it
      (incf *rule-nodes*)
      (setf new-node (make-instance 'tree-node
                       :parent     node
                       :node-data  (make-instance 'rule
                                      :parameters (if (numberp body)    ; bodyless rule?
                                                    nil
                                                    (list (car body)))  ; first element = theta
                                      :confidence (confidence formula)
                                      :literals   nil)))
      ;; Add node as current node's child
      (nconc (node-data node) (list new-node))
      ;; Prepare priority list item:
      (setf depth (depth best))
      (if (numberp body)
        (setf score  (* (confidence formula) (- 1.0 (/ depth *max-depth*))))
        (setf score  (* (confidence formula) (- 1.0 (/ depth *max-depth*))
                                             (- 1.0 (/ (length body) max-rule-length))
                                             (if (> (top-index formula) 0)
                                               abduction-penalty
                                               1.0))))
      (setf new-state (make-instance 'p-list-item
                        :score     score
                        :depth     (+ 1 depth)
                        :list-data formula
                        :ptr       new-node))
      (nconc new-states-list (list new-state)))))
(defun process-fact (node formula best)
  (****DEBUG 1 "process-fact: processing fact formula: ~a" (head formula))
  ;; Try to unify the fact with the parent node (the latter is a sub-goal):
  (****DEBUG 1 "process-fact: to unify with parent: ~a" (car (node-data (parent node))))
  (setf sub (unify (head formula) (car (node-data (parent node)))))
  ;; If unify fails, retract the proof tree node
  (if (eq sub 'fail)
    (progn
      (decf *fact-nodes*)
      (retract node best)
      (return-from process-fact)))
  ;; If unify succeeds:
  (****DEBUG 1 "process-fact: substitution = ~a" sub)
  ;; Store the (sub, message) pair in the list of solutions in the current node
  ;; Here we pass the first message from a leaf node
  ;; Note that there is an implicit factor node below this leaf, but it simply passes the
  ;;    fuzzy value of the node to its parent.  (first TV) = fuzzy value
  (setf current-solution (make-instance 'solution
                             :sub sub
                             :message (first (tv formula))))
  ;; The current node is filled with the literal (this is useful for abduction; see below)
  (setf (node-data node) (list 'fact (head formula)))
  ;; Record the node for abduction
  ;(setf *current-explanation* (list 'fact (head formula)))
  ;; Propagate the TV bottom-up, recursively
  (propagate node (list current-solution)))
(defun process-rule (node formula best)
  (****DEBUG 1 "process-rule: expanding rule formula ~a <- ~a" (head formula) (body formula))
  (setf head       (head formula)
        body       (body formula)
        top-index  (top-index formula))
  ;; At this point, a designated part of the rule (let's call it 'top') is already (partly)
  ;;   matched with the parent.  The parent is a subgoal.  We need to perform unify on them.
  (****DEBUG 1 "process-rule: to unify with parent: ~a" (car (node-data (parent node))))
  ;; top = the rule's actual "head" per this instantiation
  ;; if top-index > 0, top is a body literal; otherwise top is a head literal
  (if (> top-index 0)
    (setf top (nth (+ top-index (/ (- (length body) 1) 2)) body))
    (setf top (nth (- top-index)   head)))
  (****DEBUG 1 "process-rule: to unify with index: ~a, top: ~a" top-index top)
  (setf sub (unify top (car (node-data (parent node)))))
  (****DEBUG 1 "process-rule: substitution = ~a" sub)
  ;; If unify fails, retract proof tree node
  (if (eq sub 'fail)
    (progn
      (decf *rule-nodes*)
      (retract node best)
      (return-from process-rule)))
  ;; If unify succeeds:
  ;; We don't have to apply sub to the rule's top b/c the top is identical with the parent
  ;;    and is not stored in the rule.
  ;; If the rule is body-less, we immediately have a solution that should be propagated up
  ;;    to the parent:
  (if (numberp body)
    (progn
      (setf current-solution (make-instance 'solution
                                  :sub sub
                                  :message  body))                  ; the TV is the body
      (propagate node (list current-solution))
      (return-from process-rule)))
  (if (> top-index 0)
    (setf (node-type node) #\B)
    (setf (node-type node) #\H))
  ;; For each parameter c or body literal...
  (setf top-index2 1)
  (dolist (arg (cdr body))
    ;; Auxiliary parameters?
    (if (floatp arg)
      ;; Yes, add them to parameter list
      ;; The original order should be preserved regardless of where top-index is.
      (setf (parameters (node-data node)) (append (parameters (node-data node)) (list arg)))
      ;; For each literal:
      (progn
        (if (not (equal top-index top-index2))
          (progn
            ;; Apply substitution to literal:
            (setf lit (do-subst arg sub))
        ;; Create new nodes for the proof-tree, corresponding to arguments of the rule:
        ;; The operator and confidence are already in the proof-tree node
            (incf *goal-nodes*)
        (setf new-lit (make-instance 'tree-node :parent node))
        ;; Add new lit to current node's literals list
        (setf (literals (node-data node)) (append (literals (node-data node)) (list new-lit)))
        ;; Prepare item for priority list:
        ;; note:  depth need not increase
        ;;        score is inherited from the rule's, multiplied by a consecutive decay factor
        (nconc new-states-list (list (make-instance 'p-list-item
                                       :score     (* args-decay (score best))
                                       :depth     (depth best)
                                       :list-data lit
                                           :data-type #\B               ; #\B = body literal
                                           :ptr       new-lit)))))
        (incf top-index2))))
  ;; For each head literal...
  (setf top-index2 0)              ; head index counts from 0 downwards
  (dolist (arg head)
    (if (not (equal top-index top-index2))
      (progn
        ;; Apply substitution to literal
        (setf lit (do-subst arg sub))
        ;; Create new node
        (incf *goal-nodes*)
        (setf new-lit (make-instance 'tree-node :parent node))
        ;; Add new lit to current node's literals list
        (setf (literals (node-data node)) (append (literals (node-data node)) (list new-lit)))
        ;; Prepare item for priority list
        (nconc new-states-list (list (make-instance 'p-list-item
                                           :score     (* args-decay (score best))
                                           :depth     (depth best)
                                           :list-data lit
                                           :data-type #\H               ; #\H = head literal
                                           :ptr       new-lit)))))
    (decf top-index2)))
(defun retract (node best)
  (****DEBUG 1 "retracting node...")
  (setf parent (parent node))
  ;; The root node failed?
  (if (null parent)
    (progn
      (setf (solutions proof-tree) 'fail)
      (return-from retract)))
  ;; Is parent a rule or subgoal?
  (if (listp (node-data parent))
    ;; If parent is a sub-goal:  delete the node itself (ie the parent's child);
    ;; If the parent becomes empty then recurse
    (progn
      ;; Delete the node:  destructively modify the parent
      ;(decf *rule-nodes*)
      (setf (node-data parent) (delete node (node-data parent) :count 1))
      ;; Is parent empty now?  If so, delete it too
      (if (null (third (node-data parent)))       ; third item is the subgoal's first argument
        ;; recurse
        (retract parent best))
    )
    ;; If parent is a rule: delete parent
    (retract parent best)
  ))
(defun propagate (node new-solutions)
  (****DEBUG 1 "propagate: evaluating node: data = ~a" (node-data node))
  (setf parent    (parent node))
  ;; Add the new solutions to the current node
  (setf (solutions node) (append (solutions node) new-solutions))
  ;; Reached root node?
  (if (null parent)
    (progn
      (****DEBUG 1 "propagate: got solution = ~a" (print-solutions new-solutions))
      (return-from propagate)))
  (setf parent-data (node-data parent))
  ;; Is parent a sub-goal?
  (if (listp parent-data)
    ;; 3-6. Simply send all solutions to parent
    ;;      It seems that only the new solutions need to be sent up
    (propagate parent new-solutions)
    ;; ELSE parent is a rule:  try to apply the rule
    (let* ((rule     parent-data)
           (cR       (confidence rule))
           (theta    (car (parameters rule)))
           (literals (literals   rule)))
      (****DEBUG 1 "propagate: evaluating rule... op = ~a" theta)
      (****DEBUG 1 "propagate: rule type = ~a" (node-type parent))
      (if (equal (node-type parent) #\H)
        (tagbody
        case1A
          ;; case 1A: all Bi's are available
          ;; The strategy is to apply the AND/OR *pairwise*, sequentially.
          (setf c_list (cdr (parameters rule)))         ; List of parameters c1,c2,...
          (setf c1 (car c_list))                        ; Set c1
          (setf c_list (cdr c_list))                    ; Point to next element
          ;; First set up the left operand, X1
          (setf X1 (car literals))                      ; X1 = the first literal
          (if (eql X1 node)                             ; Is it the node with new solutions?
              (setf X1-solutions new-solutions)         ; If so, process new-solutions *only*
            (setf X1-solutions (solutions X1)))         ; If not, get the set of old solutions
          (prepare-left-operand c1 X1-solutions)        ; Prepare X1
          (****DEBUG 1 "propagate: literals = ~a" literals)
          ;; For each of the rest of siblings, X2:
          (dolist (X2 (cdr literals))
            ;; If some silbing has null TV, this means case 1A is broken, try case 1B
            (if (null (solutions x2))
              (go case1B))
            ;; If this is a "head" literal, it means the end of body literals, signals success
            (if (equal (node-type X2) #\H)
              (return))
            (if (eql X2 node)                           ; Is it the node with new solutions?
                (setf X2-solutions new-solutions)       ; If so, process new-solutions *only*
              (setf X2-solutions (solutions X2)))       ; If not, get the set of old solutions
            (setf c2 (car c_list))                      ; Set c2
            (setf c_list (cdr c_list))                  ; Point to next element
            ;; Merge with sibling's sequence:
            (setf X1-solutions
              (merge-solutions c2 X1-solutions X2-solutions))
            ;; Abduction mode?  **** TO-DO: abduction is unfinished
            (if *abduct-mode*
              ;; record the 2 arguments as parts of a potential explanation
              (setf *current-explanation* (list op *current-explanation* arg))))
      ;; If new-solutions is not empty
          (if (not (null X1-solutions))
            (progn
              ;; apply "theta":  result = (1 - theta) * AND-factor + theta * OR-factor
              (apply-theta theta X1-solutions)
              (****DEBUG 1 "propagate: got first TV = ~a" (message (car X1-solutions)))
              ;; send new-solutions to parent node;  recurse
              (propagate parent X1-solutions)))
          (return-from propagate)
        case1B
          ;; Case 1B:  apply Bayes rule (as in case 2) and then do forward (as in case 1A)
          (****DEBUG 1 "propagate: case 1B")
        )
        ;; case 2:  reverse direction; apply Bayes rule
        (progn
          (****DEBUG 1 "propagate: case 2")
          (setf c_list (cdr (parameters rule)))         ; List of parameters c1,c2,...
          (setf c1 (car c_list))                        ; Set c1
          (setf c_list (cdr c_list))                    ; Point to next element
          ;; First set up the left operand, X1
          (setf X1 (car literals))                      ; X1 = the first literal
          (if (eql X1 node)                             ; Is it the node with new solutions?
              (setf X1-solutions new-solutions)         ; If so, process new-solutions *only*
            (setf X1-solutions (solutions X1)))         ; If not, get the set of old solutions
          (prepare-left-operand c1 X1-solutions)        ; Prepare X1
          ;; For each of the rest of siblings, X2:
          (dolist (X2 (cdr literals))
            ;; If some silbing has null TV, this means case 2 is broken
            (if (null (solutions X2))
              (return-from propagate))
            ;; break out of loop if "head" literal is reached
            (if (equal (node-type X2) #\H)
              (return))
            (if (eql X2 node)                           ; Is it the node with new solutions?
                (setf X2-solutions new-solutions)       ; If so, process new-solutions *only*
              (setf X2-solutions (solutions X2)))       ; If not, get the set of old solutions
            (setf c2 (car c_list))                      ; Set c2
            (setf c_list (cdr c_list))                  ; Point to next element
            ;; Merge with sibling's sequence:
            (setf X1-solutions
              (merge-solutions c2 X1-solutions X2-solutions)))
          ;; Now find a head with solution
              
          ;; If new-solutions is not empty
          (if (not (null X1-solutions))
            (progn
              ;; apply "theta":  result = (1 - theta) * AND-factor + theta * OR-factor
              (apply-theta theta X1-solutions)
              (****DEBUG 1 "propagate: got first TV = ~a" (message (car X1-solutions)))
          ;; send new-solutions to parent node;  recurse
              (propagate parent X1-solutions))))
))))
;;; Merge two lists of solutions
;;; INPUT:   two lists of solutions
;;;          if merging is OK, perform sum-product operation
;;; OUTPUT:  new list of merged solutions
(defun merge-solutions (c2 soln-list1 soln-list2)
  (setf result-solution-list nil)
  ;; Pick an element from list1
  (dolist (solution1 soln-list1)
    ;; Add the following results to the total results
    (setf result-solution-list (nconc result-solution-list
      ;; Try to merge element1 with list2 elements
      (merge-single-solution c2 solution1 soln-list2))))
  ;; return the results
  result-solution-list)

;;; Merge a single solution against a list of solutions
(defun merge-single-solution (c2 solution1 soln-list2)
  (setf soln-list0 nil)
  (dolist (solution2 soln-list2)
    (block outer-loop
      (setf sub1 (sub solution1)
            sub2 (sub solution2))
      ;; Merge sub1 and sub2 to give sub0, avoiding duplicates
      ;;   First add to sub0 elements in sub1 that are not in sub2
      (setf sub0 nil)
      (dolist (pair1 sub1)
        (dolist (pair2 sub2)
          ;; var1 and var2 are different
          (if (not (equal (car pair1) (car pair2)))
            ;; add the pair to the result (sub0)
            (push pair1 sub0)
            ;; var1 and var2 are same:
            ;; sub1 and sub2 NOT equal?
            (if (not (equal (cdr pair1) (cdr pair2)))
              ;; break from the loop
              (return-from outer-loop)
              ;; otherwise omit this pair, do nothing
              ))))
      ;; Then add the remaining elements in sub2 to sub0
      (setf sub0 (nconc sub0 sub2))
      ;; now sub0 has the new merged sub
      ;; calculate:  X0 = sum-product X1 X2
      (setf x0 (sum-product c2 (message solution1) (message solution2)))
      ;; Create solution0 with the new X0
      (setf solution0 (make-instance 'solution
                              :sub sub0
                              :message x0))
      ;; add solution0 to soln-list0
      (push solution0 soln-list0)))
      ;; return the new list
      soln-list0)
