;;;; Tuples for Genifer logic
;;;; ========================================================
;;;; Genifer logic has only 2 operations, namely composition and pairing (or tuples).  They obey axioms that can be easily understood as multiplication (=composition) and addition (= pairing), with multiplication being non-commutative.  Using the distributive law we can reduce all formulas to their normal form, ie, a sum of products.
;;;; In Clojure, we implement tuples as sets and compositions as lists.  This is justified because for tuples, a+a = a, so they behave like sets.

(ns genifer.tuples
	(:require [clojure.set :as set :only union])
)
(declare normalize multiple-concat)

;; ***** Normalize to "sum of products" form
;; INPUT:	a term that may contain tuples
;; -- for example:				john * gives * (to mary + ball)
;;	is represented as:		(john gives #{ (to mary), (ball) } )
;;	which can be reduced to:	#{  (john gives to mary) ,
;;						      (john gives ball)  }
(defn normalize [term]
	;; scan the list and fork, add context
	(cond
	(empty? term)
		()
	(set? (first term))
		(let [reduct (reduce-tuples (rest term))]
			(set/union
				(multiple-concat (first  (first term)) reduct)
				(multiple-concat (second (first term)) reduct)))
	:else
		(multiple-concat (list (first term))
			(reduce-tuples (rest term)))))

;; **** Concatenates head to each tail
;; INPUT:		head = a term = a list
;;			tails = a set of terms = a set of lists, OR
;;				  a single term = a list
;; OUTPUT:	a set of new terms, OR a single term
(defn multiple-concat [head tails]
	(cond
	(empty? tails)
		(list head)
	(set? tails)
		(set			; convert back to set
			(for [tail tails]
				(concat head tail)))
	:else
		(concat head tail)))