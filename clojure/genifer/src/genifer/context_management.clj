;;;; Context management
;;;; ========================================================
;; Maintain a record of the contexts of all formulas in KB.
;; Each formula has a context which is a sequence of integers.
;; We need to know		1) which context has been occupied,
;; 					2) which formula has which context,
;;					3) how to generate a unique sub-context when needed.
;; A useful fact:  the entire set of contexts has the structure of a tree.
;; Each formula refers to a node in this tree (via the index which is a sequence of integers).

(ns genifer.context_management)
(declare add-new-to-context)

;; **** Context store
;; A formula is represented by its numeric ID
(def context-tree (agent
	;; Some random examples:
	'{:formulas (0) :children
		[{:formulas (1000 1001)}
		 {:formulas (2000) :children
			[{:formulas (3000)} {:formulas (4000)}]}]}
))

;; **** Add a formula to context-tree, *under* the given context, generating a unique sub-context for it
;; INPUT:		formula's ID, an int
;;			index = sequence of integers referring to positions in the tree, for example [4,7,2] refers to the 5th node's 8th child's 3rd child.  Index digits start with 0.
;; OUTPUT:	last-digit (if success), or false
;;			where last-digit should be appended to the old context of the formula
(defn add-in-context
([index formula]								; called with default arguments
	(let [[new-tree last-digit] (add-in-context @context-tree index formula)]
		(send context-tree (fn [_] new-tree))
		last-digit))

; Worker function:  returns	[ new-tree, last-digit ]
;; The algorithm is essentially the same as clojure's assoc-in function, which is 3 lines long
([tree index formula]							; called with full arguments
	(cond
	(empty? index)
		(if (empty? (:children tree))
			;; If parent has no children, create children list with single formula
				[(assoc tree :children [{:formula (list formula)}])
				0]								; return last-digit index
			;; If parent already has children, add unique child to the list
				[(assoc tree :children
					(conj (:children tree) {:formula (list formula)}))
				(count (:children tree))])		; return last-digit index
	(empty? tree)
		false
	:else
		(let [[new-subtree last-digit] (add-in-context
											(get (:children tree) (first index))
											(rest index)
											formula)]
			[(assoc tree :children
				(assoc (:children tree) (first index) new-subtree))
			last-digit]))))

;; **** Compare 2 contexts
;; 1	inclusion
;; 0	equal
;; -1	inclusion (reversed)

;; ====== Notes ========
; On IRC #Clojure, <tmciver> advices:  a tree structure using maps might look something like:
; {:formulas 3 :children [{:formulas 2} {:formulas 1}]}

; (defn build-tree
  ; "Builds a tree data structure by associating the keyword :children in map node using function get-children"
  ; [node get-children]
  ; (let [walk (fn walk
               ; [node]
               ; (let [children (get-children node)]
                 ; (assoc node :children (map walk children))))]
    ; (walk node)))
