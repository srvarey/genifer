;;;; Genifer/forward-chaining.clj
;;;;
;;;; Copyright (C) 2009 General Intelligence
;;;; All Rights Reserved
;;;;
;;;; Written by Steven Kane, YKY
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
;;;; ***** Forward-Chaining
;;;; Algorithm:
;;;;   0.  REPEAT until no new conclusion can be generated
;;;;   1.      FOR each rule in KB
;;;;   2.          IF  rule can applied, apply it
;;;;   3.              store the new conclusion in KB
;;;;
;;;; How to apply a rule?
;;;;   1.  FOR each literal in the body of the rule
;;;;   2.      search for a fact that may unify with the literal
;;;;   3.  IF  every literal in the body can be unified with a fact
;;;;   4.      THEN the head is a valid conclusion;  add it to KB
;;;;

