;;; Genifer I/O functions
;;; ==========================================================

(ns genifer.io
	(:require [clojure.string :as string :only [split]])
	(:require [clojure.set :as set :only union])
	(:gen-class)
)
(declare formularize build-formula build-formula-1 replace-nums pretty-formula)

;; INPUT:  links = a sequence of target nodes from the source nodes 1,2,3,...
;;			represented as a string such as "2,3,4,5"
;; -- The input is supplied by the GUI, from a graphical representation of natural-language sentences
(defn formularize [links words]
	(let [	words2	(remove #(= % "")
						(string/split words #","))
			pairs	(map list
						(map #(Integer/parseInt %)
							(remove #(= % "")
								(string/split links #",")))
						(range))
			formula1 (build-formula pairs ())
			formula2 (replace-nums words2 formula1)
			formula3 (pretty-formula formula2) ]
		formula3))

;; ***** Glue together a formula from a list of pairs
;; For example, { (C,A) (Y,M) (M,C) } would result in (Y*M*C*A).
;; Sums can appear within parentheses, such as (Y*M*C*{A+B}).  Note that sums are enclosed as sets.
;; Note also that, in our logic, distribution of multiplication can only occur on the left, such as C*{A+B}, but never {A+B}*C.
;; Algorithm:
;; 0.	If all pairs used, return result
;; 1.	Take the first pair
;; 2.	If formula is empty, begin it using the first pair
;; 3.	else if first pair occurs in current formula:
;; 4.		join it with current formula
;; 5.		else try next pair in list, move 1st pair to end of list
;; 5.	recurse
(defn build-formula [pairs formula]
	(cond
	(empty? pairs)
		formula
	(empty? formula)
		;; -1 means the 1st pair is pointing to nothing
		(if (== -1 (first (first pairs)))
			(build-formula (rest pairs) (list (second (first pairs))))
			(build-formula (rest pairs) (first pairs)))
	(== -1 (first (first pairs)))
		;; ignore this pair (-1 means source is pointing to nothing)
		(build-formula (rest pairs) formula)
	:else
		(let [new-formula (build-formula-1 (first pairs) formula ())]
			(println "pair = " (first pairs))
			(print "formula = ") (prn formula)
			(println "new formula=" new-formula)
			(if (= new-formula formula)
				(build-formula
						;; Try next pair, move 1st pair to end of list
						(concat (rest pairs) (list (first pairs)))
						new-formula)
				(build-formula
						(rest pairs)
						new-formula)))))

;; ***** Grow formula using 1 pair
;; Assume the pair is (A,B)
;; 0.	if current position is a set
;; 1.		recurse into set
;; 2.	if current position is a sequence
;; 3.		recurse into seq
;; 4.	if current position matches A
;; 5.		if following position is null, append with B
;; 6.		if tail is not null:
;; 7.			if tail is already a set, add B to the set
;; 8.			else open parenthesis (ie, start a new set)
;; 9.	if current position matches B
;; 10.		pre-pend term with A
;; 11.	else if no match:
;; 12.		try next position
(defn build-formula-1 [pair formula head]
	(let [	a		(first  pair)
			b		(second pair)
			current (first formula)
			tail	(rest  formula) ]
		(cond
		(empty? formula)
			head
		(set? current)
			;; Note that a set has nothing following it, because distribution is always on the left
			(concat head (list (set
				(build-formula-1 pair (seq current) ()))))
		(seq? current)
			(build-formula-1 pair tail
				;; The new head:
				(concat head (list
					(build-formula-1 pair current ()))))
		;; At this point formula must be an atom
		(= a current)
			(if (empty? tail)
				(concat head (list current b))
				(if (set? tail)
					(concat head
						(list current (set/union (list b) tail)))
					(concat head
						(list current #{ (list b) tail }))))
		(= b current)
			;; Head should be empty here, else we have an error
			(cons a formula)
		:else
			;; Try next position
			(build-formula-1 pair tail
				(concat head (list current))))))

;; **** Replace numbers (0,1,2, ...) with words
;; INPUT:	words = a list of strings
(defn replace-nums [words formula]
	(map #(cond
			(number? %)
				(nth words %)
			(set? %)
				(set (replace-nums words %))
			:else
				(replace-nums words %))
		formula))

;; ***** Pretty-print formula
(defn pretty-formula [formula]
	(cond
	(empty? formula)
		""
	(string? formula)
		formula
	(set? formula)
		(str "{"
			(apply str
				(interpose "+" (map pretty-formula formula)))
			"}")
	(seq? formula)
		(str "("
			(apply str
				(interpose "*" (map #(pretty-formula %) formula)))
			")")
	:else
		(println (type formula))))