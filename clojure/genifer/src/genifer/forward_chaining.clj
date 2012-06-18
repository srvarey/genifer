;;; Forward-chaining
;;; ==========================================================
;;; Algorithm:
;;;   0.  REPEAT until no new conclusion can be generated
;;;   1.      FOR each rule in KB
;;;   2.          IF  rule can be applied, apply it
;;;   3.              store the new conclusion in KB
;;;
;;; How to apply a rule?
;;;   1.  FOR each literal in the body of the rule
;;;   2.      search for a fact that may unify with the literal
;;;   3.  IF  every literal in the body can be unified with a fact
;;;   4.      THEN the head is a valid conclusion;
;;;				  apply substitutions to the head and add it to KB

(ns genifer.forward_chaining
	(:require	[genifer.unification				:as unify]
				[genifer.substitution				:as subst]
				[genifer.knowledge_representation	:as knowledge]
				[clojure.math.combinatorics 		:as combinatorics]
				[clojure.string						:as string] )
)

(declare forward-chain solve-rule solve-goal)

;; For parallel execution -- not used yet
;(import '(java.util.concurrent Executors ExecutorCompletionService))
;(def executor (Executors/newCachedThreadPool))

(defn forward-chain [incoming]
	;; On entry, the incoming fact is added to working memory
	(send-off knowledge/working-mem conj incoming)
	(let [	result1 (string/join (list "Added: " (prn-str incoming)))
	;; Match new fact with rules;  will use indexed fetch in the future
	;; Find all rules that matches incoming
			result2 (apply concat
				(for [rule knowledge/rules]
					(for [sub (solve-rule rule)]		; For each solution
						(println-str (subst/substitute sub (first rule))))))]
		(string/join "\n" (list result1 (string/join "\n" result2)))))

;; Try to satisfy rule with facts in KB
;; OUTPUT:  a list of compound subs
(defn solve-rule [rule]
	(let [	body (rest rule)
			;; solutions1 = list of list of compound subs
			;; solutions2 = cartesian product of 1 = list of list of compound subs
			;; For each list of compound subs, semantics is AND
			;;   so each list is flattened to atomic subs => solutions3
			;;   and then the atomic subs are checked against each other for compatibility
			solutions1 (map solve-goal body)
			solutions2 (apply combinatorics/cartesian-product solutions1)
			solutions3 (map #(apply concat %)		; flatten the list
				(map #(map seq %) solutions2))]	; convert compound subs to seqs
			;;solutions4 (map distinct solutions3) 	; remove duplicates (optional)
	    (filter subst/compatible? solutions3)))

;; Find all facts that satisfy literal
;; OUTPUT:  a list of compound subs
(defn solve-goal [goal]
	(apply concat				; flatten results of map
		(remove false?
			(map #(unify/unify % goal) @knowledge/working-mem))))
