;;; Genifer /narrowing.clj
;;;
;;; Copyright (C) General Intelligence
;;; All Rights Reserved
;;;
;;; Written by YKY
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
;;; ***** Narrowing = unification modulo a rewriting system

(ns genifer.narrowing
	(:require [genifer.unification				:as unify])
	(:require [genifer.substitution				:as subst])
	(:require [genifer.knowledge_representation	:as knowledge])
)

(import '(java.util.concurrent Executors ExecutorCompletionService))
(def executor2
    "No harm in sharing one executor for all races."
	(Executors/newCachedThreadPool))
(def comp-service (ExecutorCompletionService. executor2))

(declare narrow rewrite find-sublist check-sublist fetch-rewrite-rule)

;; Narrowing:  unify 2 terms, 'left' and 'right', modulo a rewriting system
;; The algorithm starts with the pair {left =? right} and incrementally "narrows" the equation by applying *one* rewrite rule either to left or right.  Continue such a sequence, until it ends with an equation {left* =? right*} that can be syntactically unified, thus returning success.
;; So this is a search procedure that depends on which rewrite rule we choose to apply at each juncture.  Branching factor may be extremely high due to the size of the rewrite system, and the fact that rewriting can occur at different positions inside a term!
;; In order to optimize, we should 1) avoid "narrowing at variables", 2) search the rewrite system by the terms-to-be-unified at current positions, 3) allow rewriting only in an oriented direction.
;; 1. See if unify(left, right) succeeds, if so return solution
;; 2. For each of 'left' and 'right':
;; 3.	For each position within the term:
;; 4.		Find rewrite rules that may apply
;; 5.		Try unify sub-term with left side of rewrite rule
;; 6.		If unifiable, apply sub to both left and right, then apply rewrite rule
;; 7.		Spawn sub-processes to continue search (recurse)
;; OUTPUT:  a list of compound subs
(defn narrow
([left right] (narrow left right ()))			; call with default arguments
([left right subs]								; full set of arguments
	(let [sub (unify/unify left right)]
		(if (not (false? sub))
			(concat (list sub) subs)			; Success, return sub + subs
			;; Else: try rewriting -- don't wanna indent

	;; Pick a term to try;  term2 will be the 'other' term, ie dummy
	(doseq [[term term2] [[left right] [right left]]]
		(doseq [position (range (count term))]	; for each position in term
			;; Unless the position is at a variable, then rewriting possibilities are unlimited...  What do we do in this case???
			;; Find rewrite rules that may apply
			(let [	sub-term	(nthrest term position)
					rules		(fetch-rewrite-rule (first sub-term))]
				(doseq [rule rules]
					;; Try unify
					(let [sub (unify/unify sub-term (first rule))]
						(if (false? sub)
							false
							(let [	;; apply subs to term and dummy
									term*	(subst/substitute sub term)
									term2*	(subst/substitute sub term2)
									;; rewrite the term
									term**	(rewrite (first rule) (second rule) term*)
									;; recurse and add the new sub to subs collection
									futures (.submit comp-service
										#(narrow term** term2* (concat sub subs)))
									solution (first (drop-while false?
										(.get (.take comp-service))))]
								;; Cancel remaining tasks
								(doseq [future futures]
									(.cancel future true))
								solution)))))))))))

;; Search term for old, replace with new
;; -- Cannot use substitute because "old" can consist of multiple atoms.
;; -- A problem arises when rewrite is possible at multiple positions;  Now we assume we can rewrite at the leftmost position and the search will find the other positions later.
(defn rewrite [old new term]
	(let [index (find-sublist old term)]
		(concat
			(take index term)
			new
			(nthrest term (+ index (count old))))))

;; Find position of sublist in list
(defn find-sublist [sublist list]
	(if (empty? list)
		false
		(let [position	(.indexOf list (first sublist))]
			(if (check-sublist sublist (nthrest list position))
				; if yes return position, end-position
				position
				(find-sublist sublist (nthrest list (inc position)))))))

(defn check-sublist [sublist list]
	(if (empty? list)
		(if (empty? sublist)
			true
			false)
		(if (empty? sublist)
			true
			(if (= (first sublist) (first list))
				(check-sublist (rest sublist) (rest list))
				false))))

;; Select rules that have a common first element with key
;; -- Rewriting should only occur in the preferred direction.
(defn fetch-rewrite-rule [key]
	(filter
		#(= key (first (first %)))
		knowledge/rewrite-sys))
