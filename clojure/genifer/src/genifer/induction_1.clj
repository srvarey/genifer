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
;;; 1. rlgg (Plotkin 1970, is dual to unification)
;;; 2. Lopster (Lapointe & Matwin 1992, based on inverse implication)
;;; 3. Clint (de Raedt 1991)
;;; 4. Golem (Muggleton 1990, based on rlgg)
;;; 5. Cigol (Muggleton & Buntine 1988, based on inverse resolution)

(ns genifer.induction_1)

(declare anti-unify)

;; Let's implement the algorithm from "Simply Logical" by Peter Flach (1994)

;; Anti-unification = calculate lgg of 2 terms;  lgg = least general generalization
;; 
(defn anti-unify [t1 t2]
)
