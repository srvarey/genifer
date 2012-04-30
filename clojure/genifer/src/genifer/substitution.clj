;;; Genifer /unification.clj
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
;;; ***** Syntactic unification, without rewriting yet

(ns genifer.substitution)
(declare substitute substitute-atomic compatible? atomic-compatible?)

;; Apply a compound substitution to a term
;; INPUT: sub = compound sub
;;		 term, as list
;; OUTPUT: term
(defn substitute [subs term]
	(if (empty? subs)
		term
		(substitute-atomic (first subs)
			(substitute (rest subs) term))))

;; Make an atomic substitution
(defn substitute-atomic [sub term]
	(if (empty? term)
		()
		(let [old (first sub)
			  new (rest sub)]
			;; Scan term for old
			;; If match, replace with new
			(concat
				(if (= (first term) old)
					new
					(list (first term)))
				(substitute-atomic sub (rest term))))))

;; Test a list of atomic subs against each other
;; Returns:  true if compatible, false if not
(defn compatible? [subs-list]
	(every? #(= % true)
		(for [sub1 subs-list		; This generates all possible pairs
			  sub2 subs-list]
			(atomic-compatible? sub1 sub2))))

;; Check if 2 atomic subs are compatible
;; Returns:  true if OK,  false if incompatible
(defn atomic-compatible? [sub1 sub2]
	(or
		;; Same variables?  If not that's good
		(not= (first sub1) (first sub2))
		;; Otherwise, if substituents are the same that's OK too
		(= (rest sub1) (rest sub2))))
