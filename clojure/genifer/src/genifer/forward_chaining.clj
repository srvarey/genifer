;;; Genifer/forward-chaining.clj
;;;
;;; Copyright (C) General Intelligence
;;; All Rights Reserved
;;;
;;; Written by Steven Kane, YKY
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
;;; ***** Forward-chaining
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
	(:require [genifer.unification				:as unify])
	(:require [genifer.substitution				:as subst])
	(:require [genifer.knowledge_representation :as knowledge])
	(:use [clojure.math.combinatorics :only [cartesian-product]])
)

(declare forward satisfy-rule satisfy-goal)

;; For parallel execution -- not used yet
;(import '(java.util.concurrent Executors ExecutorCompletionService))
;(def executor (Executors/newCachedThreadPool))

(defn forward [incoming]
	;; On entry, the incoming fact is added to working memory
	(println "Adding: " incoming)
	(send knowledge/work-mem conj incoming)
	;; Match new fact with rules;  will use indexed fetch in the future
	;; Find all rules that matches incoming
	(doseq [rule knowledge/rules]
		(doseq [sub (satisfy-rule rule)]		; For each solution
			(println (subst/substitute sub (first rule))))))

;; Try to satisfy rule with facts in KB
;; Returns a list of compound subs
(defn satisfy-rule [rule]
	(let [	body (rest rule)
			;; solutions1 = list of list of compound subs
			;; solutions2 = cartesian product of 1 = list of list of compound subs
			;; For each list of compound subs, semantics is AND
			;;   so each list is flattened to atomic subs => solutions3
			;;   and then the atomic subs are checked against each other for compatibility
			solutions1 (map satisfy-goal body)
			solutions2 (apply cartesian-product solutions1)
			solutions3 (map #(apply concat %)		; flatten the list
				(map #(map seq %) solutions2))]	; convert compound subs to seqs
			;;solutions4 (map distinct solutions3) 	; remove duplicates (optional)
	    (filter subst/compatible? solutions3)))

;; Find all facts that satisfy literal
;; Returns a list of compound subs
(defn satisfy-goal [goal]
	(apply concat				; flatten results of map
		(remove false?
			(map #(unify/unify % goal) @knowledge/work-mem))))
