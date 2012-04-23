;;; Genifer /unify.clj
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

(ns genifer)

(require '[clojure.set :as set])		; for set/union

(declare unify unify2 const? variable? fork-subs add-sub)

;; Syntactic unify
;; input: 2 terms t1 and t2
;; output: a substitution unifying t1, t2
;;         false if fail
(defn unify [t1 t2]
	(unify2 t1 t2 0 []))

;; Main algorithm, will be explained in detail in the book
;; -- at any point, a variable on one side would be consuming input on the other side
;; -- direction = which side has a consuming variable: 0 = none, 1 = left, -1 = right
;; -- sub = the partial substitution of the consuming variable
;; -- a substitution is a list [X,A,B,C...] representing { ABC... / X }
;; OUTPUT: a set (#{...}) of compound substitutions
(defn unify2 [t1 t2 direction sub]
	(let [a1 (first t1)
		  a2 (first t2)
		  r1 (rest t1)
		  r2 (rest t2)]
		;(println "unify: " a1 a2 r1 r2 direction)

		;; If either side is exhausted:
		(cond
		(and (nil? a1) (nil? a2))
			(reverse sub)			; success, regardless of direction
		(nil? a1)
			(if (== direction 1)
				(unify2 t1 r2 1 (cons a2 sub))
				(if (const? a2)
					false
					(add-sub sub
						(unify2 t1 r2 -1 [a2]))))
		(nil? a2)
			(if (== direction -1)
				(unify2 r1 t2 -1 (cons a1 sub))
				(if (const? a1)
					false
					(add-sub sub
						(unify2 r1 t2 1 [a1]))))
		:else		; continue to main course, don't wanna ident

		(case direction
		;; Fresh position: neither side is consuming
		0	(cond
			(and (const? a1) (const? a2) (= a1 a2))
				(unify2 r1 r2 0 [])
			(and (const? a1) (const? a2) (not= a1 a2))
				false
			(and (variable? a1) (const? a2))
				(fork-subs
					(add-sub [a1]
						(unify2 r1 t2 0 []))
					;; Begin substitution { a2... / a1 }
					(unify2 r1 r2 1 [a2, a1]))
			(and (const? a1) (variable? a2))
				(fork-subs
					(add-sub [a2]
						(unify2 t1 r2 0 []))
					;; Begin substitution { a1... / a2 }
					(unify2 r1 r2 -1 [a1, a2]))
			:else
				;; Either a1 consumes a2 or vice versa
				(fork-subs (unify2 r1 r2  1 [a2, a1])
					   (unify2 r1 r2 -1 [a1, a2])))
		;; A variable (X) on the left is consuming
		1	(cond
			(and (const? a1) (const? a2) (= a1 a2))
				(fork-subs
					;; X ends and return to freshness
					(add-sub sub
						(unify2 r1 r2 0 []))
					;; X consumes a2
					(unify2 t1 r2 1 (cons a2 sub)))
			(and (const? a1) (const? a2) (not= a1 a2))
				;; X must consume a2
				(unify2 t1 r2 1 (cons a2 sub))
			(and (const? a1) (variable? a2))
				(fork-subs
					;; X consumes a2
					(unify2 t1 r2 1 (cons a2 sub))
					;; X ends, a2 consumes a1
					(add-sub sub
						(unify2 r1 r2 -1 [a1, a2])))
			(and (variable? a1) (const? a2))
				(fork-subs
					;; X consumes a2
					(unify2 t1 r2 1 (cons a2 sub))
					;; X ends, a1 consumes a2
					(add-sub sub
						(unify2 r1 r2 1 [a2, a1])))
			:else
				;; X consumes a2, a1 consumes a2, or a2 consumes a1
				(fork-subs
					(unify2 t1 r2 1 (cons a2 sub))
					(add-sub sub
						(unify2 r1 r2 1 [a2, a1]))
					(add-sub sub
						(unify2 r1 r2 -1 [a1, a2]))))
		;; A variable (Y) on the right is consuming -- mirrors case 1
		-1	(cond
			(and (const? a1) (const? a2) (= a1 a2))
				(fork-subs
					;; Y ends and return to freshness
					(add-sub sub
						(unify2 r1 r2 0 []))
					;; Y consumes a1
					(unify2 r1 t2 -1 (cons a1 sub)))
			(and (const? a1) (const? a2) (not= a1 a2))
				;; Y must consume a1
				(unify2 r1 t2 -1 (cons a1 sub))
			(and (const? a1) (variable? a2))
				(fork-subs
					;; Y consumes a1
					(unify2 r1 t2 -1 (cons a1 sub))
					;; Y ends, a2 consumes a1
					(add-sub sub
						(unify2 r1 r2 -1 [a1, a2])))
			(and (variable? a1) (const? a2))
				(fork-subs
					;; Y consumes a1
					(unify2 r1 t2 -1 (cons a1 sub))
					;; Y ends, a1 consumes a2
					(add-sub sub
						(unify2 r1 r2 1 [a2, a1])))
			:else
				;; Y consumes a1, a1 consumes a2, or a2 consumes a1
				(fork-subs
					(unify2 r1 t2 -1 (cons a1 sub))
					(add-sub sub
						(unify2 r1 r2 1 [a2, a1]))
					(add-sub sub
						(unify2 r1 r2 -1 [a1, a2]))))
		))))

;; Is x a constant?  Yes if name of x begins with lower-case		
(defn const? [x]
	(Character/isLowerCase (first (name x))))

;; Is x a variable?  Yes if name of x begins with upper-case
(defn variable? [x]
	(Character/isUpperCase (first (name x))))

;; Add a compound substitution to the set of compound substitutions
;; -- semantics is OR
(defn fork-subs [x y]
	;(println "fork: " x ", " y)
	(cond
	(false? x)
		y
	(false? y)
		x
	:else
		(set/union #{x} #{y})))

;; Add an atomic substitution to a compound substitution
;; -- semantics is AND
;; -- on input, x needs to be reversed because it was built up via cons
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
		(reverse x)
	:else
		(set/union #{(reverse x)} #{y})))

(def tests '[
	[john loves mary]   	[john loves mary]
		[]
	[john loves mary]   	[john hates mary]
		false
	[john loves mary X]		[john loves mary obsessively]
		[X obsessively]
	[john loves X]      	[john loves mary obsessively]
		[X mary obsessively]
	[john X]            	[john loves mary obsessively]
		[X loves mary obsessively]
	[john X possessively]	[john loves mary obsessively]
		false
	[john X obsessively]	[john loves mary obsessively]
		[X loves mary]
	[X]						[love]
		[X love]
	[love]                 	[Y]
		[Y love]
	[]						[love]
		false
	[X loves mary]			[john loves mary]
		[X john]
	[X loves Y]				[john loves mary]
		#{[X john] [Y mary]}
	[X loves Y Z]			[john loves mary]
		#{ #{[X john] [Y mary] [Z]}
	       #{[X john] [Y] [Z mary]} }
	[X loves Y Z]			[john loves mary obsessively]
		#{ #{[X john] [Y mary] [Z obsessively]}
	       #{[X john] [Y mary obsessively] [Z]}
	       #{[X john] [Y] [Z mary obsessively]} }
	])

(defn run-tests []
	(loop [test tests]
		(let [t1 (first test)
			  t2 (second test)
			  answer (nth test 2)]
			(println "Expected: " answer)
			(println "     ===> " (unify t1 t2))
			(println)
			(if (not (empty? (rest (rest (rest test)))))
				(recur (rest (rest (rest test))))))))