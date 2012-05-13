;;; Special unification for narrowing
;;; ==========================================================
;;; (unify-R t1,t2) unifies flexibly on t1 till t1 is exhausted
;;; (unify-L t1,t2) unifies flexibly on t1 in the reverse direction
;;; Both are designed to make unify more efficient for narrowing, nothing more

(ns genifer.unification2
	(:require [clojure.set :as set]))		; for set/union

(declare unify-L unify-R const? variable? fork-subs add-sub add-sub-L rectify-sub)

;; **** unify-R t1,t2
;; -- t1 is "favored" in the sense that unify-R ends flexibly whenever t1 is exhausted
;; -- all changes from old unify:
;;		-- if either side not exhausted, no change
;;		-- if t1 is exhausted, return success (marked with ** comment)
;;		-- if t2 is exhausted, return failure (no change)
;; input: 2 terms t1 and t2
;; output: a list of compound substitutions, each compound sub can unify t1, t2
;;         false if no sub can be found
(defn unify-R
([t1 t2]								; call with default arguments
	(unify-R t1 t2 0 ()))

;; OUTPUT: a list of compound substitutions, each compound substitution is a set, ie, #{...}
([t1 t2 direction sub]					; with full set of arguments
	(let [a1 (first t1)
		  a2 (first t2)
		  r1 (rest t1)
		  r2 (rest t2)]

		;; If either side is exhausted:
		(cond
		(nil? a1)						; ** success regardless of a2 or direction
			(list #{(reverse sub)})		; ** return
		(nil? a2)
			(if (== direction -1)
				(unify-R r1 t2 -1 (cons a1 sub))
				(if (const? a1)
					false
					(add-sub sub
						(unify-R r1 t2 1 (list a1)))))
		:else		; continue to main course, don't wanna ident

		(case direction
		;; Fresh position: neither side is consuming
		0	(cond
			(and (const? a1) (const? a2) (= a1 a2))
				(unify-R r1 r2 0 [])
			(and (const? a1) (const? a2) (not= a1 a2))
				false
			(and (variable? a1) (const? a2))
				(fork-subs
					(add-sub (list a1)
						(unify-R r1 t2 0 ()))
					;; Begin substitution { a2... / a1 }
					(unify-R r1 r2 1 (list a2,a1)))
			(and (const? a1) (variable? a2))
				(fork-subs
					(add-sub [a2]
						(unify-R t1 r2 0 ()))
					;; Begin substitution { a1... / a2 }
					(unify-R r1 r2 -1 (list a1,a2)))
			:else
				;; Either a1 consumes a2 or vice versa
				(fork-subs (unify-R r1 r2  1 (list a2,a1))
					   (unify-R r1 r2 -1 (list a1,a2))))
		;; A variable (X) on the left is consuming
		1	(cond
			(and (const? a1) (const? a2) (= a1 a2))
				(fork-subs
					;; X ends and return to freshness
					(add-sub sub
						(unify-R r1 r2 0 ()))
					;; X consumes a2
					(unify-R t1 r2 1 (cons a2 sub)))
			(and (const? a1) (const? a2) (not= a1 a2))
				;; X must consume a2
				(unify-R t1 r2 1 (cons a2 sub))
			(and (const? a1) (variable? a2))
				(fork-subs
					;; X consumes a2
					(unify-R t1 r2 1 (cons a2 sub))
					;; X ends, a2 consumes a1
					(add-sub sub
						(unify-R r1 r2 -1 (list a1,a2))))
			(and (variable? a1) (const? a2))
				(fork-subs
					;; X consumes a2
					(unify-R t1 r2 1 (cons a2 sub))
					;; X ends, a1 consumes a2
					(add-sub sub
						(unify-R r1 r2 1 (list a2,a1))))
			:else
				;; X consumes a2, OR a1 consumes a2, OR a2 consumes a1
				(fork-subs
					(unify-R t1 r2 1 (cons a2 sub))
					(fork-subs
						(add-sub sub
							(unify-R r1 r2 1 (list a2,a1)))
						(add-sub sub
							(unify-R r1 r2 -1 (list a1,a2))))))
		;; A variable (Y) on the right is consuming -- mirrors case 1
		-1	(cond
			(and (const? a1) (const? a2) (= a1 a2))
				(fork-subs
					;; Y ends and return to freshness
					(add-sub sub
						(unify-R r1 r2 0 ()))
					;; Y consumes a1
					(unify-R r1 t2 -1 (cons a1 sub)))
			(and (const? a1) (const? a2) (not= a1 a2))
				;; Y must consume a1
				(unify-R r1 t2 -1 (cons a1 sub))
			(and (const? a1) (variable? a2))
				(fork-subs
					;; Y consumes a1
					(unify-R r1 t2 -1 (cons a1 sub))
					;; Y ends, a2 consumes a1
					(add-sub sub
						(unify-R r1 r2 -1 (list a1,a2))))
			(and (variable? a1) (const? a2))
				(fork-subs
					;; Y consumes a1
					(unify-R r1 t2 -1 (cons a1 sub))
					;; Y ends, a1 consumes a2
					(add-sub sub
						(unify-R r1 r2 1 (list a2,a1))))
			:else
				;; Y consumes a1, a1 consumes a2, or a2 consumes a1
				(fork-subs
					(unify-R r1 t2 -1 (cons a1 sub))
					(fork-subs
						(add-sub sub
							(unify-R r1 r2 1 (list a2,a1)))
						(add-sub sub
							(unify-R r1 r2 -1 (list a1,a2))))))
		)))))

;; **** unify-L t1,t2
;; Three changes from old unify need to be made:
;; -- Again, t1 is "favored" in the sense that it can be exhausted at any time;  this causes the change commented by **.
;; -- Direction of consuming is reversed -- that means the subs must be reversed upon completion, which is perfect because the subs are originally built up reversed via cons! (commented by ***)  However we need to move the substituting variable from the end of list to the front, this is done by rectify-sub.
;; -- add-sub is changed to add-sub-L, for the reason above.
;; input: 2 terms t1 and t2
;; output: a list of compound substitutions, each compound sub can unify t1, t2
;;         false if no sub can be found
(defn unify-L
([t1 t2]								; if called with default arguments
	(unify-L t1 t2 0 ()))

;; OUTPUT: a list of compound substitutions, each compound substitution is a set, ie, #{...}
([t1 t2 direction sub]					; with full set of arguments
	(let [a1 (first t1)
		  a2 (first t2)
		  r1 (rest t1)
		  r2 (rest t2)]

		;; If either side is exhausted:
		(cond
		(nil? a1)						; ** success, regardless of a2 and direction
			(list #{(rectify-sub sub)})	; *** no reverse
		(nil? a2)
			(if (== direction -1)
				(unify-L r1 t2 -1 (cons a1 sub))
				(if (const? a1)
					false
					(add-sub-L sub
						(unify-L r1 t2 1 (list a1)))))
		:else		; continue to main course, don't wanna ident

		(case direction
		;; Fresh position: neither side is consuming
		0	(cond
			(and (const? a1) (const? a2) (= a1 a2))
				(unify-L r1 r2 0 [])
			(and (const? a1) (const? a2) (not= a1 a2))
				false
			(and (variable? a1) (const? a2))
				(fork-subs
					(add-sub-L (list a1)
						(unify-L r1 t2 0 ()))
					;; Begin substitution { a2... / a1 }
					(unify-L r1 r2 1 (list a2,a1)))
			(and (const? a1) (variable? a2))
				(fork-subs
					(add-sub-L [a2]
						(unify-L t1 r2 0 ()))
					;; Begin substitution { a1... / a2 }
					(unify-L r1 r2 -1 (list a1,a2)))
			:else
				;; Either a1 consumes a2 or vice versa
				(fork-subs (unify-L r1 r2  1 (list a2,a1))
					   (unify-L r1 r2 -1 (list a1,a2))))
		;; A variable (X) on the left is consuming
		1	(cond
			(and (const? a1) (const? a2) (= a1 a2))
				(fork-subs
					;; X ends and return to freshness
					(add-sub-L sub
						(unify-L r1 r2 0 ()))
					;; X consumes a2
					(unify-L t1 r2 1 (cons a2 sub)))
			(and (const? a1) (const? a2) (not= a1 a2))
				;; X must consume a2
				(unify-L t1 r2 1 (cons a2 sub))
			(and (const? a1) (variable? a2))
				(fork-subs
					;; X consumes a2
					(unify-L t1 r2 1 (cons a2 sub))
					;; X ends, a2 consumes a1
					(add-sub-L sub
						(unify-L r1 r2 -1 (list a1,a2))))
			(and (variable? a1) (const? a2))
				(fork-subs
					;; X consumes a2
					(unify-L t1 r2 1 (cons a2 sub))
					;; X ends, a1 consumes a2
					(add-sub-L sub
						(unify-L r1 r2 1 (list a2,a1))))
			:else
				;; X consumes a2, OR a1 consumes a2, OR a2 consumes a1
				(fork-subs
					(unify-L t1 r2 1 (cons a2 sub))
					(fork-subs
						(add-sub-L sub
							(unify-L r1 r2 1 (list a2,a1)))
						(add-sub-L sub
							(unify-L r1 r2 -1 (list a1,a2))))))
		;; A variable (Y) on the right is consuming -- mirrors case 1
		-1	(cond
			(and (const? a1) (const? a2) (= a1 a2))
				(fork-subs
					;; Y ends and return to freshness
					(add-sub-L sub
						(unify-L r1 r2 0 ()))
					;; Y consumes a1
					(unify-L r1 t2 -1 (cons a1 sub)))
			(and (const? a1) (const? a2) (not= a1 a2))
				;; Y must consume a1
				(unify-L r1 t2 -1 (cons a1 sub))
			(and (const? a1) (variable? a2))
				(fork-subs
					;; Y consumes a1
					(unify-L r1 t2 -1 (cons a1 sub))
					;; Y ends, a2 consumes a1
					(add-sub-L sub
						(unify-L r1 r2 -1 (list a1,a2))))
			(and (variable? a1) (const? a2))
				(fork-subs
					;; Y consumes a1
					(unify-L r1 t2 -1 (cons a1 sub))
					;; Y ends, a1 consumes a2
					(add-sub-L sub
						(unify-L r1 r2 1 (list a2,a1))))
			:else
				;; Y consumes a1, a1 consumes a2, or a2 consumes a1
				(fork-subs
					(unify-L r1 t2 -1 (cons a1 sub))
					(fork-subs
						(add-sub-L sub
							(unify-L r1 r2 1 (list a2,a1)))
						(add-sub-L sub
							(unify-L r1 r2 -1 (list a1,a2))))))
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

;; Special version of add-sub that leaves the subs reversed, for use in unify-L
(defn add-sub-L [x y]
	(cond
	(false? x)
		false
	(false? y)
		false
	(empty? x)
		y
	(empty? y)
		(list #{(rectify-sub x)})			; *** no reverse
	:else
		(for [y1 y]							; for each compound sub in y
			(if (empty? (first y1))
				#{(rectify-sub x)}			; *** no reverse
				;; do the set union
				(set/union #{(rectify-sub x)} y1)))))	; *** no reverse

;; sub_ is of the form (a b c ... X)
;; OUTPUT:  a real substitution of the form (X a b c...)
(defn rectify-sub [sub_]
	(if (empty? sub_)
		()
		(cons (last sub_) (butlast sub_))))