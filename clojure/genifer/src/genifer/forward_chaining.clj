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
;;; ***** Forward-Chaining
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
	(:require [genifer.unification  :as unify])
	(:require [genifer.substitution :as subst])
	(:require [genifer.knowledge    :as knowledge])
	(:use [clojure.math.combinatorics :only [cartesian-product]])
)

(declare forward satisfy-rule satisfy-literal)

;; For parallel execution -- not used yet
  (import '(java.util.concurrent Executors ExecutorCompletionService))
  (def executor (Executors/newCachedThreadPool))

(defn forward [incoming]
	;; On entry, the incoming fact is added to working memory
	(println "Adding: " incoming)
	(send knowledge/work-mem conj incoming)
	;; Match new fact with rules, using indexed fetch
	;; Find all rules that matches incoming
	(doseq [rule knowledge/rules]
		(doseq [sub (satisfy-rule rule)]		; For each solution
			(println (subst/substitute sub (first rule))))))

;; Try to satisfy rule with facts in KB
;; Returns a list of compound subs
(defn satisfy-rule [rule]
	(let [body (rest rule)
		  subs-list (map satisfy-literal body)	; list of lists of compound subs
		  solutions (apply cartesian-product subs-list)
		  ;; solutions = list of lists of compound subs
		  ;; Semantics is AND -- each list of compounds are flattened to atomic subs
		  ;;  and then the atomic subs are checked against each other
		  solutions2 (map #(apply concat %)		; flatten the seqs
				(map #(map seq %) solutions))	; convert compound subs to seqs
		  solutions3 (map distinct solutions2)] ; remove duplicates
	    (filter subst/compatible? solutions3)))

;; Find all facts that satisfy literal
;; Returns a list of compound subs
(defn satisfy-literal [literal]
	(apply concat				; flatten results of map
		(remove false?
			(map #(unify/unify % literal) @knowledge/work-mem))))
