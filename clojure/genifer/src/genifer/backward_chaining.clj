;;; Genifer /backward_chaining.clj
;;;
;;; Copyright (C) General Intelligence
;;; All Rights Reserved
;;;
;;; Written by William Taysom, YKY
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Affero General Public License v3 as
;;; published by the Free Software Foundation and including the exceptions
;;; at http://opencog.org/wiki/Licenses
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with this program; if not, write to:
;;; Free Software Foundation, Inc.,
;;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; ==========================================================
;;; ***** Backward chaining
;;; -- implemented as a parallel search
;;; -- the search tree interleaves calling of solve-goal and solve-rule
;;; -- solveGoal takes the first result available (OR)
;;; -- solveRule collects all results before calculating an answer (AND)
;;; -- technically this is called an AND-OR tree
;;;
;;; For example, to solve a goal G, we may have 2 rules:
;;;   G <- A, B, C           (rule 1)
;;;   G <- D, E, F, G.       (rule 2)
;;; Both rules are applicable (ie, could provide a correct answer),
;;;   so we can accept either one (OR).
;;; For each rule, we have to wait till all the sub-goals come back
;;;   in order to calculate a result, so it is a conjunction (AND).
;;;
;;; This type of AND-OR tree search arises naturally in all rule-based /
;;;   logical systems.  Some variations are possible (for example, wait
;;;   for a specific time and then choose the best answer for OR-parallelism),
;;;   but the basic structure is the same.
;;; ------------------------------------------------------

(ns genifer.backward_chaining
	(:require [genifer.unification				:as unify])
	(:require [genifer.substitution				:as subst])
	(:require [genifer.knowledge_representation	:as knowledge])
	(:use [clojure.math.combinatorics :only [cartesian-product]])
)

(import '(java.util.concurrent Executors ExecutorCompletionService))
(def executor
    "No harm in sharing one executor for all races."
	(Executors/newCachedThreadPool))

(declare solve-goal solve-rule skip-while match-facts match-rules match-1-rule)

;; Find all facts (in working memory) that unifies with goal.
;; Because our logic allows rewriting, it is difficult to predict from syntax alone which facts will unify with goal.  So we are forced to try unify with all facts (at least in working memory).  In the future we can use contexts to select subsets of the KB to try unify.
;; OUTPUT:  list of compound subs, could be ()
(defn match-facts [goal]
	(apply concat
		(remove false?
			(map #(unify/unify % goal) @knowledge/work-mem))))

;; Find rules that unifies with goal
;; OUTPUT:	a lazy sequence of rule bodies with subs applied, can be ()
(defn match-rules [goal]
	;; For each rule, unify rule head with goal, if fail then nothing,
	;; Otherwise apply subs to rule body
	(apply concat
		(map #(match-1-rule % goal) knowledge/rules)))

;; Try to match one rule, if success apply substitution to rule body(s) and return them, otherwise return false.
;; OUTPUT:  list of rule bodies, can be ()
(defn match-1-rule [rule goal]
	(let [subs (unify/unify (first rule) goal)]		; subs = list of compound subs
		(if (false? subs)
			()
			(for [sub subs]
				(map #(subst/substitute sub %) (rest rule))))))

;; For facts, we just return the subs (and the TVs if fuzzy-probabilistic)
;; For rules, we find the rules that unify with the goal via some subs, apply those subs to the rule's premises (sub-goals), and solve the sub-goals recursively.
;; Then we get the sequence of subs, check compatibility, and return viable solutions.
;; OUTPUT:  list of compound substitutions, or ()
(defn solve-goal [goal]
	(let [solutions (match-facts goal)]
		(if (not (empty? solutions))
			solutions		; return answers
			;; Else -- don't wanna indent

	(let [rule-bodies (match-rules goal)]
		(if (empty? rule-bodies)
			()		; no applicable rules, return ()
			;; Spawn new concurrent processes
			(let [	comp-service (ExecutorCompletionService. executor)
					futures (doseq [rule-body rule-bodies]
								(.submit comp-service #(solve-rule rule-body)))
				  ;; Get the 1st result that's not ()
					solution (first (drop-while empty? (.get (.take comp-service))))]
				;; Cancel remaining tasks
				(doseq [future futures]
					(.cancel future true))
				solution))))))

;; INPUT:	rule body = list of literals to be satisfied
;; OUTPUT:	list of compound substitutions (and truth values), can be ()
;; -- This function is analogous to satisfy-rule in forward_chaining.  The rule body has a bunch of literals to be satisfied.  We need to test the compatibility of all combinations of solutions to each literal, hence the Cartesian product is used.  Imagine each literal has a lazy sequence of solutions attached to it, like vertical sausages.  After the Cartesian product we have a sequence of horizontal sausages.
(defn solve-rule [body]
	(let [solutions1 (pmap solve-goal body)]			; note use of parallel map
		;; solutions1 is a list of lists of compound subs
		(if (some empty? solutions1)					; if some sub-goals failed
			()
			;; else: merge solutions and return only compatible ones
			(let [	solutions2 (apply cartesian-product solutions1)
					solutions3 (map #(apply concat %)	; flatten the list
						(map #(map seq %) solutions2))]	; convert compound subs to seqs
				(filter subst/compatible? solutions3)))))
