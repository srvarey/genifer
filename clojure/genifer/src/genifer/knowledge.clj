;;; Genifer/knowledge.clj
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

(ns genifer.knowledge)

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
;; -- implemented as a hash-table, ie, a hash-map with vector values
;; (get {:a 1 :a 2 :b 3} :a) ?
;; (merge-with vector {:a 1 :b 2} {:a 1})
;; ==> {:a [1 1], :b 2}
;(def rules (hash-map 'goal '[premises]))
(def rules '(
;;	Conclusion        <=== 	Premises .....
	((X and Y are happy)	(X loves Y) (Y loves X))
))

;; Working memory
;; -- just a list of facts
;; -- use clojure "agent" model because transactions can be asynchronous & uncoordinated
(def work-mem (agent
'(
	(john loves mary)
	(john loves jane)
	(pete loves ann)
	(paul loves ann)
	(ann loves paul)
)))
