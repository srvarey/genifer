;;; Genifer I/O functions
;;; ==========================================================

(ns genifer.io
	(:require [clojure.string :as string :only [split]])
	(:gen-class)
)
(declare formularize form-formula tail-of)

;; INPUT:  links = a sequence of target nodes from the source nodes 1,2,3,...
;;			represented as a string such as "2,3,4,5"
(defn formularize [links]
	(let [	pairs	(map list
						(map #(Integer/parseInt %)
							(remove #(= % "")
								(string/split links #",")))
						(range))
			formula (form-formula pairs ()) ]
		(println-str formula)))

;; ***** Glue together a formula from a list of pairs
;; For example, { (C,A) (Y,M) (M,C) } would result in (Y*M*C*A).
;; Sums can appear within parentheses, such as (Y*M*C*(A+B)).
;; Note also that, in our logic, distribution of multiplication can only occur on the left, such as C*(A+B), but never (A+B)*C.
;; Algorithm:
;; 1.	Take the first pair
;; 2.	If formula is empty, begin it using the first pair
;; 3.	else if first pair occurs in current formula:
;; 4.		join it with current formula
;; 5.		else try next pair in list, move 1st pair to end of list
;; 5.	recurse
(defn form-formula [pairs formula]
	(cond
	(empty? pairs)
		formula
	(empty? formula)
		(form-formula (rest pairs) (first pairs))
	:else
		(let [	a		(first  (first pairs))
				b		(second (first pairs))
				index1 (.indexOf formula a)
				index2 (.indexOf formula b) ]
			(println "pair = " a b)
			(println "formula = " formula)
			(cond
			(>= index1 0)
				;; put 2nd element into formula at index+1, if clash then open parenthesis
				(if (= (count formula) (+ index1 1))
					;; If already at end of formula
					(form-formula (rest pairs) (concat formula (list b)))
					;; else, open parenthesis
					(form-formula (rest pairs)
						(concat
							(take (+ index1 1) formula)
							(list (list b (tail-of formula index1))))))
			(>= index2 0)
				;; Must add 'a' to beginning of formula, because distribution is always on the left
				(form-formula (rest pairs) (cons a formula))
			:else
				(form-formula (concat (rest pairs) (list (first pairs))) formula)))))

;; Get the tail of formula after index
;; If the tail is a single element, return it as a single item, otherwise the result is a list
(defn tail-of [formula index]
	(if (= (count formula) (+ 2 index))
		(last formula)
		(nthrest formula (+ 1 index))))
