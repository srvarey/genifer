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

(ns genifer.narrowing)

;; Narrowing:  unify 2 terms, left and right, modulo a rewriting system
;; The algorithm starts with the pair {left =? right} and incrementally "narrows" the equation by applying 1 rewrite rule either to left or right.  Continue such a sequence, until it ends with an equation {left' =? right'} that can be syntactically unified, thus returning success.
;; So this is a search procedure that depends on which rewrite rule we choose to apply at each juncture.  Branching factor may be very high due to the size of the rewrite system.
;; 0. Repeat:
;; 1.	See if unify(left, right) can succeed, if so return solution
;; 2.	Apply a rewrite rule on either left or right
(defn narrow [left right]
	;; repeat:
	;; try (unify/unify left right)
	;; find a rewrite rule to apply:  (doseq [rule rewrite-sys] ...
	)
