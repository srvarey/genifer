;;;; Genifer/unification.clj
;;;;
;;;; Copyright (C) 2009 Genint
;;;; All Rights Reserved
;;;;
;;;; Written by YKY
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
;;;; **** The 'unify' function is modified from Peter Novig's paper:
;;;; "Correcting A Widespread Error in Unification Algorithms"
;;;;    Software — Practice & Experience, vol 21:2, Feb 1991, p231-233
;;;; Novig's code is ported to Clojure by Kevin Livingston on:
;;;;     http://groups.google.com/group/clojure/browse_thread/thread/996ecadf98328c6b#

(declare unify-variable
         variable?
         occurs-in?
         bound?
         lookup
         extend-subst)

(defn unify
  ([x y] (unify x y {}))
  ([x y subst]
  (cond
    (= x y) subst
    (= subst 'fail) 'fail
    (variable? x) (unify-variable x y subst)
    (variable? y) (unify-variable y x subst)
    (or  (empty? x) (empty? y) (not (seq? x)) (not (seq? y)))
      'fail
    :else (unify (rest x) (rest y)
            (unify (first x) (first y) subst)))))

(defn unify-variable [var val subst]
  "Unify var with val, using (and possibly extending) subst."
  (println "unify-var:" var "," val "," subst)
  (cond
    (= var val) subst
    (bound? var subst)
      (unify (lookup var subst) val subst)
    ;; New condition: dereference val when it is a variable
    (and (variable? val) (bound? val subst))
      (unify var (lookup val subst) subst)
    (occurs-in? var val subst)
      'fail
    :else (extend-subst var val subst)))

;; Use this if you want to suppress occurs-check
; (defn occurs-in? [var x subst]
;   false)

(defn occurs-in? [var x subst]
  "Does var occur anywhere inside x?"
  (cond
    (= var x) true
    (bound? x subst)
      (occurs-in? var (lookup x subst) subst)
    (and (seq? x) (not (empty? x)))
      (or (occurs-in? var (first x) subst)
          (occurs-in? var (rest x) subst))
    :else false))

(defn bound? [x subst]
  "Is x a bound variable?"
  (contains? subst x))

(defn lookup [var subst]
  (get subst var))

(defn extend-subst [var val subst]
  (println "extend-subst" var "," val "," subst)
  (assoc subst var val))

(defn variable? [x]
  (and (symbol? x)
       (= (first (name x)) \?)))   ; variable names start with '?'

;;; ===============================================================================
;;; The following function is copied from [Steven L Tanimoto 1990]
;;; "The Elements of Artificial Intelligence -- Using Common Lisp", 1st ed, p230-232

;;; Apply substitution to exp
(defn do-subst [exp sub]
  (cond (empty? sub) exp
        :else (replace (rest (first sub))
                  (ffirst sub)
                  (do-subst exp (rest sub)))))

; ----------------------------------------------------------------------------------------------
; -------------------------------- UNFINISHED BELOW THIS LINE ----------------------------------
; ----------------------------------------------------------------------------------------------

;;; =====================================================================
;;; **** Factoring and Standardizing Apart
;;; (Standardizing apart is a trivial case of factoring)
;;; Find unifiable literals within a clause
;;; and make sure that c1 and c2 share no variables
;;; -- Maybe we can temporarily forget factoring?
;;;    assume there are no duplicate literals in a clause

;;; This is an index by which we create new unique variables as ?[index]
(def *unique-var-index* 1000)

;;; For accumulating results in find-all-vars
(def *new-vars* nil)

; ;; Find all variables in clause
; (defn find-all-vars [term]
;   (if (empty? term)
;     (return-from find-all-vars))
;   (if (list? term)
;     (do
;       (find-all-vars (first term))
;       (if (not (empty? (rest term)))
;         (find-all-vars (rest term))))
;     (if (symbol? term)
;         (if (= (first (name term)) \?)
;           (pushnew term *new-vars*)))))

;;; INPUT:  c2 = head of clause
;;;         c3 = body of clause
;;;         The head and body of clause is split because that's how we store them in rules
;;; OUTPUT: a list of substitutions such that, when applied, c2 and c3 will have fresh variables
;;; Algorithm:
;;; 1. find all vars in c2, c3
;;; 2. create unique subs for the vars

; (defn standardize-apart [c2 c3]
;   ;; Results will be accumulated in this variable:
;   (set *new-vars* nil)
;   (find-all-vars c2)
;   (find-all-vars c3)
;   (create-unique-subs *new-vars*))

; (defn create-unique-subs [clashed-vars]
;   (mapcar
;     (lambda (v) (cons v (intern (concatenate 'string "?"
;                                    (write-to-string (incf *unique-var-index*))))))
;     clashed-vars))

