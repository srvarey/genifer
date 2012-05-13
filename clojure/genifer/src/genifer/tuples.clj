;;;; Tuples for Genifer logic
;;;; ========================================================
;;;; Genifer logic has only one operation, namely composition.  However, tuples can be added to the logic as syntactic sugar.  Tuples such as (a,b) obey this rule of reduction:  a*(b,c) = (a*b, a*c),  where * is composition.

(ns genifer.tuples)
(declare reduce-tuples multiple-concat)

;; INPUT:		term that may contain tuples
;; -- for example:				john * gives * (to mary, ball)
;;	is represented as:		(john gives [ (to mary), (ball) ])
;;	which can be reduced to:	(  (john gives to mary)
;;						   (john gives ball)  )
(defn reduce-tuples [term]
	;; scan the list and fork, add context
	(cond
	(empty? term)
		()
	(vector? (first term))
		(let [reduct (reduce-tuples (rest term))]
			(concat
				(multiple-concat (first  (first term)) reduct)
				(multiple-concat (second (first term)) reduct)))
	:else
		(multiple-concat (list (first term))
			(reduce-tuples (rest term)))))

;; **** Concatenates head to each tail
;; INPUT:		head = a term = a list
;;			tails = a list of terms = a list of lists
;; OUTPUT:	a list of new terms
(defn multiple-concat [head tails]
	(if (empty? tails)
		(list head)
		(for [tail tails]
			(concat head tail))))