;;; Genifer /induction_1.clj
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
;;; ***** Inductive learner #1 (bottom-up)

;;; Some historical bottom-up learners:
;;; 1. rlgg (Plotkin 1970, = anti-unification)
;;; 2. Lopster (Lapointe & Matwin 1992, based on inverse implication)
;;; 3. Clint (de Raedt 1991)
;;; 4. Golem (Muggleton 1990, based on rlgg)
;;; 5. Cigol (Muggleton & Buntine 1988, based on inverse resolution)

(ns genifer.induction_1)

(require '[clojure.set :as set])		; for set/union

(declare anti-unify anti-unify2 const? variable? add-sub fork-subs)

;; Let's implement the bottom-up learner from "Simply Logical" by Peter Flach (1994)

;; Anti-unification forms the basis of generalizing from examples in a bottom-up manner.
;; Unification and anti-unification are dual to each other, both searching the lattice spanning the general-to-specific order.  Unification finds the "greatest lower bound" (aka "most general unifier"), whereas anti-unification finds the "least upper bound" (aka "least general generalization", or lgg).
;; Their algorithms are very similar. The difference is when unification crashes, anti-unification will create new variables to absorb the conflicts.
(defn anti-unify [t1 t2]
	(anti-unify2 t1 t2 0 ()))

; Replace unify with anti-unify
;		unify2 with anti-unify2
; Seems no need to change case 1 and -1:
;	if consuming, keep consuming
;	consumption will never lead to conflicts
; Seems that all I need to do is to deal with the "false" cases
; When crash happens, need to 

;; Main algorithm, modified from unify
;; -- sub = the partial substitution of the consuming variable
;; OUTPUT: a list of compound substitutions, each compound substitution is a set, ie, #{...}
(defn anti-unify2 [t1 t2 direction sub]
	(let [a1 (first t1)
		  a2 (first t2)
		  r1 (rest t1)
		  r2 (rest t2)]
		;(println "anti-unify: " a1 a2 r1 r2 direction)

		;; If either side is exhausted:
		(cond
		(and (nil? a1) (nil? a2))
			(list #{(reverse sub)})		; success, regardless of direction
		(nil? a1)
			(if (== direction 1)
				(anti-unify2 t1 r2 1 (cons a2 sub))
				(if (const? a2)
					false
					(add-sub sub
						(anti-unify2 t1 r2 -1 (list a2)))))
		(nil? a2)
			(if (== direction -1)
				(anti-unify2 r1 t2 -1 (cons a1 sub))
				(if (const? a1)
					false
					(add-sub sub
						(anti-unify2 r1 t2 1 (list a1)))))
		:else		; continue to main course, don't wanna ident

		(case direction
		;; Fresh position: neither side is consuming
		0	(cond
			(and (const? a1) (const? a2) (= a1 a2))
				(anti-unify2 r1 r2 0 [])
			(and (const? a1) (const? a2) (not= a1 a2))
				false
			(and (variable? a1) (const? a2))
				(fork-subs
					(add-sub (list a1)
						(anti-unify2 r1 t2 0 ()))
					;; Begin substitution { a2... / a1 }
					(anti-unify2 r1 r2 1 (list a2,a1)))
			(and (const? a1) (variable? a2))
				(fork-subs
					(add-sub [a2]
						(anti-unify2 t1 r2 0 ()))
					;; Begin substitution { a1... / a2 }
					(anti-unify2 r1 r2 -1 (list a1,a2)))
			:else
				;; Either a1 consumes a2 or vice versa
				(fork-subs (anti-unify2 r1 r2  1 (list a2,a1))
					   (anti-unify2 r1 r2 -1 (list a1,a2))))
		;; A variable (X) on the left is consuming
		1	(cond
			(and (const? a1) (const? a2) (= a1 a2))
				(fork-subs
					;; X ends and return to freshness
					(add-sub sub
						(anti-unify2 r1 r2 0 ()))
					;; X consumes a2
					(anti-unify2 t1 r2 1 (cons a2 sub)))
			(and (const? a1) (const? a2) (not= a1 a2))
				;; X must consume a2
				(anti-unify2 t1 r2 1 (cons a2 sub))
			(and (const? a1) (variable? a2))
				(fork-subs
					;; X consumes a2
					(anti-unify2 t1 r2 1 (cons a2 sub))
					;; X ends, a2 consumes a1
					(add-sub sub
						(anti-unify2 r1 r2 -1 (list a1,a2))))
			(and (variable? a1) (const? a2))
				(fork-subs
					;; X consumes a2
					(anti-unify2 t1 r2 1 (cons a2 sub))
					;; X ends, a1 consumes a2
					(add-sub sub
						(anti-unify2 r1 r2 1 (list a2,a1))))
			:else
				;; X consumes a2, OR a1 consumes a2, OR a2 consumes a1
				(fork-subs
					(anti-unify2 t1 r2 1 (cons a2 sub))
					(fork-subs
						(add-sub sub
							(anti-unify2 r1 r2 1 (list a2,a1)))
						(add-sub sub
							(anti-unify2 r1 r2 -1 (list a1,a2))))))
		;; A variable (Y) on the right is consuming -- mirrors case 1
		-1	(cond
			(and (const? a1) (const? a2) (= a1 a2))
				(fork-subs
					;; Y ends and return to freshness
					(add-sub sub
						(anti-unify2 r1 r2 0 ()))
					;; Y consumes a1
					(anti-unify2 r1 t2 -1 (cons a1 sub)))
			(and (const? a1) (const? a2) (not= a1 a2))
				;; Y must consume a1
				(anti-unify2 r1 t2 -1 (cons a1 sub))
			(and (const? a1) (variable? a2))
				(fork-subs
					;; Y consumes a1
					(anti-unify2 r1 t2 -1 (cons a1 sub))
					;; Y ends, a2 consumes a1
					(add-sub sub
						(anti-unify2 r1 r2 -1 (list a1,a2))))
			(and (variable? a1) (const? a2))
				(fork-subs
					;; Y consumes a1
					(anti-unify2 r1 t2 -1 (cons a1 sub))
					;; Y ends, a1 consumes a2
					(add-sub sub
						(anti-unify2 r1 r2 1 (list a2,a1))))
			:else
				;; Y consumes a1, a1 consumes a2, or a2 consumes a1
				(fork-subs
					(anti-unify2 r1 t2 -1 (cons a1 sub))
					(fork-subs
						(add-sub sub
							(anti-unify2 r1 r2 1 (list a2,a1)))
						(add-sub sub
							(anti-unify2 r1 r2 -1 (list a1,a2))))))
		))))

;; Is x a constant?  Yes if name of x begins with lower-case		
(defn const? [x]
	(Character/isLowerCase (first (name x))))

;; Is x a variable?  Yes if name of x begins with upper-case
(defn variable? [x]
	(Character/isUpperCase (first (name x))))

;; Merge 2 lists of compound substitutions
;; -- semantics is OR
;; -- INPUT: each of x, y is a list of compound subs
;; -- a compound sub is a set
;; -- OUTPUT: a list of compound subs
(defn fork-subs [x y]
	;(println "fork: " x ", " y)
	(cond
	(false? x)
		y
	(false? y)
		x
	:else
		(concat x y)))

;; Add an atomic substitution to a list of compound substitutions
;; -- semantics is AND, distributed into the list
;; -- INPUT: x is an atomic sub, as a list (not set)
;;           y is a list of compound subs, ie, a list of sets
;;           On input, x needs to be reversed because it was built up via cons
;; -- OUTPUT: a list of compound subs
(defn add-sub [x y]
	;(println "add: " x ", " y)
	(cond
	(false? x)
		false
	(false? y)
		false
	(empty? x)
		y
	(empty? y)
		(list #{(reverse x)})
	:else
		(for [y1 y]		; for each compound sub in y
			(if (empty? (first y1))
				#{(reverse x)}
				;; do the set union
				(set/union #{(reverse x)} y1)))))
