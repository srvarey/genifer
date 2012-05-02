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
(def executor (Executors/newCachedThreadPool))

(declare narrow rewrite)

;; Narrowing:  unify 2 terms, 'left' and 'right', modulo a rewriting system
;; The algorithm starts with the pair {left =? right} and incrementally "narrows" the equation by applying *one* rewrite rule either to left or right.  Continue such a sequence, until it ends with an equation {left* =? right*} that can be syntactically unified, thus returning success.
;; So this is a search procedure that depends on which rewrite rule we choose to apply at each juncture.  Branching factor may be extremely high due to the size of the rewrite system, and the fact that rewriting can occur at different positions inside a term!
;; In order to optimize, we should avoid "narrowing at variables", and search the rewrite system by the terms-to-be-unified at current positions.
;; 1. See if unify(left, right) succeeds, if so return solution
;; 2. For each of 'left' and 'right':
;; 3.	For each position within the term:
;; 4.		Find a rewrite rule that may apply
;; 5.		Try unify sub-term with either side of rule
;; 6.		If unifiable, apply sub to both left and right, then apply rewrite rule
;; 7.		Spawn sub-processes to continue search (recurse)
(defn narrow [left right subs]
	(let [sub (unify/unify left right)]
		(if (not (false? sub))
			(concat sub	subs)		; Success, return sub + subs
			;; Else: try rewriting -- don't wanna indent

	;; Pick a term to try;  term2 will be the 'other' term, ie dummy
	(doseq [[term term2] '[[left right] [right left]]]
		(doseq [position (range	(length term))]	; for each position in term
			;; Unless the position is at a variable, then rewriting possibilities are unlimited...  What do we do in this case???
			;; Find rewrite rules that may apply
			(let [rules (fetch-rewrite-rule (sub-term position term))]
				(doseq [[old new] '[ [(first  rule) (second rule)]
									 [(second rule) (first  rule)] ]]
					;; Try unify
					(let [sub (unify/unify (sub-term position term) old)]
						(if (not (false? sub))
									;; apply subs to term and dummy
							(let [	term*	(subst/substitute sub term)
									term2*	(subst/substitute sub term2)
									;; rewrite the term
									term**	(rewrite old new term*)]
								;; recurse and add the new sub to subs collection
								(narrow term** term2* (concat sub subs))))))))))))

;; Search term for old, replace with new
;; -- Cannot use substitute because "old" can consist of multiple atoms
(defn rewrite [old new term]
)

(defn sub-term [position term]
)

(defn fetch-rewrite-rule [key]
)