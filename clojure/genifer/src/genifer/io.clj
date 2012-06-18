;;; Genifer I/O functions
;;; ==========================================================
;;; For use with Genifer GUI

(ns genifer.io
	(:require	[genifer.knowledge_representation :as knowledge]
				[clojure.string :as string]
				[cheshire.core] )
	(:gen-class))
(declare formularize build-formula build-formula-1 replace-nums simplify pretty-formula)

;; INPUT:  links = a sequence of target nodes from the source nodes 1,2,3,...
;;			represented as a string such as "2,3,4,5,"
;; -- The input is supplied by the GUI, from a graphical representation of natural-language sentences
(defn formularize [links words]
	(let [	words2	(remove #(= % "")
						(string/split words #","))
			;; Turn input string into a sequence of pairs, note that "map list" takes 2 arguments
			pairs	(map list
						(range)
						(map #(Integer/parseInt %)
							(remove #(= % "")
								(string/split links #","))))
			formula1 (build-formula pairs ())
			formula2 (replace-nums words2 formula1)
			formula3 (simplify formula2)
			output	 (pretty-formula formula3) ]
		;; Remove outer-most parentheses
		(string/join (butlast (rest output)))))

;; ***** Glue together a formula from a list of pairs
;; For example, { (C,A) (Y,M) (M,C) } would result in (Y*M*C*A).
;; Sums can appear within parentheses, such as ( {X+Y} *M*C*A).  Note that sums are enclosed as sets.
;; Note also that, in our logic, distribution of multiplication can only occur from the right, such as {X+Y} * Z, but never Z* {X+Y}.
;; Algorithm:
;; 0.	If all pairs used, return result
;; 1.	Take the first pair
;; 2.	If formula is empty, begin it using the first pair
;; 3.	else try to build formula using 1st pair
;; 4.	if formula has changed, recurse with next pair in list
;; 5.	else move 1st pair to end of list, recurse with next pair in list
(defn build-formula [pairs formula]
	(cond
	(empty? pairs)
		formula
	(empty? formula)
		;; (?,-1) means the pair is pointing to nothing
		(if (== -1 (second (first pairs)))
			(build-formula (rest pairs) (list (first (first pairs))))
			(build-formula (rest pairs) (first pairs)))
	(== -1 (second (first pairs)))
		;; ignore this pair if it is pointing to nothing)
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
;; 4.	if current position matches B
;; 5.		if head is empty, pre-pend with A
;; 6.		if head is non-empty:
;; 7.			if preceding position is already a set, add A to that set
;; 8.			else open parenthesis (ie, start a new set with A and head)
;; 9.	if current position matches A (now tail must be empty or an error has occurred)
;; 10.		append term with B
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
			;; Note that a set has nothing preceding it, because distribution is always from the right
			(build-formula-1 pair tail
				(list (set (build-formula-1 pair (seq current) ()))))
		(seq? current)
			(build-formula-1 pair tail
				;; The new head:
				(concat head (list
					(build-formula-1 pair current ()))))
		;; At this point formula must be an atom
		(= b current)
			(if (empty? head)
				(cons a formula)
				(if (set? (last head))	; we should have (first head) = (last head) here
					(concat
						(list (conj (first head) (list a)) b)
						tail)
					(concat
						(list #{ head (list a) } b)
						tail)))
		(= a current)
			;;  tail should be empty here, else we have an error
			(concat head (list a b))
		:else
			;; Try next position
			(build-formula-1 pair tail
				(concat head (list current))))))

;; Eliminate extra parentheses (optional)
(defn simplify [formula]
	(cond
	(empty? formula)
		()
	(string? formula)
		formula
	(set? formula)
		(set (map simplify formula))
	(seq? formula)
		(if (= 1 (count formula))
			(simplify (first formula))
			(map simplify formula))))

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

;; Send a KB formula as JSON string
(defn send-formula [index]
	(cheshire.core/generate-string (nth @knowledge/working-mem
		(Integer/parseInt index))))

;; Parse a JSON formula and add it to KB
(defn get-formula [s]
	(send-off knowledge/working-mem conj
		;; "true" means get back keywords as keywords, not as strings
		(cheshire.core/parse-string s true)))
