;;; Genifer/knowledge_representation.clj
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
;;; ***** Definitions of logic, knowledge representation, and memory systems

(ns genifer.knowledge_representation)

;;;; ======================== Logic ===========================
;; formula := atom |
;;            variable |
;;            atom . formula |
;;            variable . formula

;; rule := head <- body
;; head := formula
;; body := literal |
;;         literal , body
;; literal := formula
;; In other words:
;; rule := formula <- formula, formula, ...

;; So a formula would be [a b c ...]
;; A rule would be ([a b c...] [a b c...] [a b c...] ...)

;;;; ========================= KB =============================
;; KB (knowledge-base)
;; -- perhaps should be implemented as a hash-table, ie, a hash-map with vector values
;; (get {:a 1 :a 2 :b 3} :a) ?
;; (merge-with vector {:a 1 :b 2} {:a 1})
;; ==> {:a [1 1], :b 2}
;; (def rules (hash-map 'goal '[premises]))

(def rules '(
;;	Conclusion        <=== 	Premises .....
	((X and Y is happy)		(X love Y) (Y love X))
	((W is sad)				(Z hate W))
))

;; Working memory
;; -- just a list of facts
;; -- use clojure "agent" model because transactions can be asynchronous & uncoordinated
(def work-mem (agent
'(
	(john love mary)
	(john love jane)
	(pete love ann)
	(paul love ann)
	(ann love paul)
	(john hate joe)
	(superman can fly)
	(clark kent is superman)
	(rose is red)
)))

;; Rewrite systems

;; System 1 is oreinted from left to right
(def rewrite-sys1 '(
;;	Left			===>		Right
	((very X)			(X))
	((unhappy)			(not happy))
))

;; System 2 is reversible
(def rewrite-sys2 '(
;;	Left			<==>		Right
	((love)				(like))
	((jealous)			(envy))
	((sad)				(unhappy))
	((clark kent)		(superman))
))