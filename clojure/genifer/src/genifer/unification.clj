;;; Genifer /unification.clj
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
;;; ***** Syntactic unification

(ns genifer.unification
	(:require [clojure.set :as set]))		; for set/union

(declare unify const? variable? fork-subs add-sub)

;; Syntactic unify
;; input: 2 terms t1 and t2
;; output: a list of compound substitutions, each compound sub can unify t1, t2
;;         false if no sub can be found
;; -- Note: a list containing a single compound sub (#{()}) means that t1, t2 can be trivially unified, so it means success
(defn unify
([t1 t2]								; call with default arguments
	(unify t1 t2 0 ()))

;; Main algorithm, will be explained in detail in the book:
;; -- at any point, at most 1 variable on one side would be consuming input from the other side
;; -- direction = which side has a consuming variable: 0 = none, 1 = left, -1 = right
;; -- sub = the partial substitution of the consuming variable
;; -- a substitution is a list (X,A,B,C...) representing { ABC... / X }
;; OUTPUT: a list of compound substitutions, each compound substitution is a set, ie, #{...}
([t1 t2 direction sub]					; with full set of arguments
	(let [a1 (first t1)
		  a2 (first t2)
		  r1 (rest t1)
		  r2 (rest t2)]
		;(println "unify: " a1 a2 r1 r2 direction)

		;; If either side is exhausted:
		(cond
		(and (nil? a1) (nil? a2))
			(list #{(reverse sub)})		; success, regardless of direction
		(nil? a1)
			(if (== direction 1)
				(unify t1 r2 1 (cons a2 sub))
				(if (const? a2)
					false
					(add-sub sub
						(unify t1 r2 -1 (list a2)))))
		(nil? a2)
			(if (== direction -1)
				(unify r1 t2 -1 (cons a1 sub))
				(if (const? a1)
					false
					(add-sub sub
						(unify r1 t2 1 (list a1)))))
		:else		; continue to main course, don't wanna ident

		(case direction
		;; Fresh position: neither side is consuming
		0	(cond
			(and (const? a1) (const? a2) (= a1 a2))
				(unify r1 r2 0 [])
			(and (const? a1) (const? a2) (not= a1 a2))
				false
			(and (variable? a1) (const? a2))
				(fork-subs
					(add-sub (list a1)
						(unify r1 t2 0 ()))
					;; Begin substitution { a2... / a1 }
					(unify r1 r2 1 (list a2,a1)))
			(and (const? a1) (variable? a2))
				(fork-subs
					(add-sub [a2]
						(unify t1 r2 0 ()))
					;; Begin substitution { a1... / a2 }
					(unify r1 r2 -1 (list a1,a2)))
			:else
				;; Either a1 consumes a2 or vice versa
				(fork-subs (unify r1 r2  1 (list a2,a1))
					   (unify r1 r2 -1 (list a1,a2))))
		;; A variable (X) on the left is consuming
		1	(cond
			(and (const? a1) (const? a2) (= a1 a2))
				(fork-subs
					;; X ends and return to freshness
					(add-sub sub
						(unify r1 r2 0 ()))
					;; X consumes a2
					(unify t1 r2 1 (cons a2 sub)))
			(and (const? a1) (const? a2) (not= a1 a2))
				;; X must consume a2
				(unify t1 r2 1 (cons a2 sub))
			(and (const? a1) (variable? a2))
				(fork-subs
					;; X consumes a2
					(unify t1 r2 1 (cons a2 sub))
					;; X ends, a2 consumes a1
					(add-sub sub
						(unify r1 r2 -1 (list a1,a2))))
			(and (variable? a1) (const? a2))
				(fork-subs
					;; X consumes a2
					(unify t1 r2 1 (cons a2 sub))
					;; X ends, a1 consumes a2
					(add-sub sub
						(unify r1 r2 1 (list a2,a1))))
			:else
				;; X consumes a2, OR a1 consumes a2, OR a2 consumes a1
				(fork-subs
					(unify t1 r2 1 (cons a2 sub))
					(fork-subs
						(add-sub sub
							(unify r1 r2 1 (list a2,a1)))
						(add-sub sub
							(unify r1 r2 -1 (list a1,a2))))))
		;; A variable (Y) on the right is consuming -- mirrors case 1
		-1	(cond
			(and (const? a1) (const? a2) (= a1 a2))
				(fork-subs
					;; Y ends and return to freshness
					(add-sub sub
						(unify r1 r2 0 ()))
					;; Y consumes a1
					(unify r1 t2 -1 (cons a1 sub)))
			(and (const? a1) (const? a2) (not= a1 a2))
				;; Y must consume a1
				(unify r1 t2 -1 (cons a1 sub))
			(and (const? a1) (variable? a2))
				(fork-subs
					;; Y consumes a1
					(unify r1 t2 -1 (cons a1 sub))
					;; Y ends, a2 consumes a1
					(add-sub sub
						(unify r1 r2 -1 (list a1,a2))))
			(and (variable? a1) (const? a2))
				(fork-subs
					;; Y consumes a1
					(unify r1 t2 -1 (cons a1 sub))
					;; Y ends, a1 consumes a2
					(add-sub sub
						(unify r1 r2 1 (list a2,a1))))
			:else
				;; Y consumes a1, a1 consumes a2, or a2 consumes a1
				(fork-subs
					(unify r1 t2 -1 (cons a1 sub))
					(fork-subs
						(add-sub sub
							(unify r1 r2 1 (list a2,a1)))
						(add-sub sub
							(unify r1 r2 -1 (list a1,a2))))))
		)))))

;; Is x a constant?  Yes if name of x begins with lower-case		
(defn const? [x]
	(if (symbol? x)
		(Character/isLowerCase (first (name x)))
		true))

;; Is x a variable?  Yes if name of x begins with upper-case
(defn variable? [x]
	(if (symbol? x)
		 (Character/isUpperCase (first (name x)))
		false))
	
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

;; Create fresh variables in rule
;; -- this is a standard procedure in logic
;; -- every time a new rule is introduced, this must be called first to avoid variable clashes
(defn standardize-apart [rule]
	;; Create a map of fresh variable substitutions for all unique vars
	(let [subs-map (apply hash-map
		(interleave
			; find distinct vars
			(distinct (concat	(filter variable? (first  rule))
								(filter variable? (second rule))))
			(repeatedly #(gensym "V__"))))]
		;; Change all variables
		(for [term rule]				; for head and body of rule
			(for [atom term]
				(if (variable? atom)
					(subs-map atom)		; look up the map
					atom)))))
