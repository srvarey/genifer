;;; Genifer /backward_chaining.clj
;;;;
;;;; Copyright (C) General Intelligence
;;;; All Rights Reserved
;;;;
;;;; Written by William Taysom, YKY
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
;;;; ==========================================================

;; Backward chaining
;; -- implemented as a parallel search
;; -- the search tree interleaves calling of solve-goal and solve-rule
;; -- solveGoal takes the first result available (OR)
;; -- solveRule collects all results before calculating an answer (AND)
;; -- technically this is called an AND-OR tree

;; For example, to solve a goal G, we may have 2 rules:
;;   G <- A, B, C           (rule 1)
;;   G <- D, E, F, G.       (rule 2)
;; Both rules are applicable (ie, could provide a correct answer),
;;   so we can accept either one (OR).
;; For each rule, we have to wait till all the sub-goals come back
;;   in order to calculate a result, so it is a conjunction (AND).

;; This type of AND-OR tree search arises naturally in all rule-based /
;;   logical systems.  Some variations are possible (for example, wait
;;   for a specific time and then choose the best answer for OR-parallelism),
;;   but the basic structure is the same.
;; ------------------------------------------------------

(ns genifer.backward_chaining
	(:require [genifer.unification  :as unify])
	(:require [genifer.substitution :as subst])
	(:require [genifer.knowledge    :as knowledge])
)
(import '(java.util.concurrent Executors ExecutorCompletionService))

(declare start solve-goal solve-rule skip-while)

(def executor
    "No harm in sharing one executor for all races."
	(Executors/newCachedThreadPool))

(defn start []
	(loop []
		(print "Enter query: ") (flush)
		(let [query (read)
			  truth (solve-goal query)]
			(printf "Answer is: %s\n" truth))
		(recur)))

;; Find all facts (in working memory) that unifies with goal.
;; Because our logic allows rewriting, it is difficult to predict from syntax alone which facts will unify with goal.  So we are forced to try unify with all facts (at least in working memory).  In the future we can use contexts to select subsets of the KB to try unify.
;; Returns a lazy sequence of subs
(defn fetch-facts [goal]
	(remove false?
		(map #(unify/unify % goal) knowledge/work-mem)))

;; Find rules that unifies with goal, returns a lazy sequence of subs
(defn fetch-rules [goal]
	(remove false?
		(map #(unify/unify (first %) goal) knowledge/rules)))

;; For facts, we just return the subs (and the TVs if fuzzy-probabilistic)
;; For rules, we find the rules that unify with the goal via some subs, apply those subs to the rule's premises (sub-goals), and solve the sub-goals recursively.
;; Then we get the sequence of subs, check compatibility, and return viable solutions.
(defn solve-goal [goal]
	(let [solutions (fetch-facts goal)]
		(if (not (empty? solutions))
			solutions))		; return answers
	(let [rules (fetch-rules goal)]
		(cond
		(empty? rules)
			nil		; no applicable rules, return nil
		:else
			;; Spawn new concurrent processes
			(let [comp-service (ExecutorCompletionService. executor)
				  futures (doseq [rule rules]
						(.submit comp-service #(solve-rule rule)))
				  ;; Get the 1st result that's not nil
				  result (skip-while nil? #(.get (.take comp-service)))]
				;; Cancel remaining tasks
				(doseq [future futures]
					(.cancel future true))
				result))))

;; Keeps calling fun until (pred fun) is false
(defn skip-while [pred fun]
	(let [result (apply fun [])]
		(if (pred result)
			(skip-while pred fun)
			result)))

(defn solve-rule [rule-tail]
	(let [truths (pmap solve-goal rule-tail)]
		(if (some nil? truths)		; if some sub-goals failed
			nil
			(apply + truths))))		; sum up truth values -- for testing
