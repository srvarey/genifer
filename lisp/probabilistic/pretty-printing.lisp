;;;; =======================================================================
;;;;      functions for printing the proof tree and priority list (in deduction)
;;;; =======================================================================

;;;; Genifer/deduction.lisp
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
;;;; ------------------------------------------------------------------------------

(defun print-priority-list ()
  (****DEBUG 1 "Priority list:")
  (dolist (item priority-list)
    (if (listp (list-data item))
      (****DEBUG 1 "    <~a,~a> ~a" (score item) (depth item) (list-data item))
      (****DEBUG 1 "    <~a,~a> ~a <- ~a" (score item) (depth item)
                                        (head (list-data item)) (body (list-data item))))))

(defun print-proof-tree ()
  (****DEBUG 1 "Proof tree:")
  (print-tree-node "" proof-tree)
  (****DEBUG 1 "^^^^^^^^^^^^^^^^^^^^"))

(defun print-tree-node (spaces node)
  (if (eql node 'fail)
    (****DEBUG 1 "~a_fact: <fail>" spaces)
    (if (null node)
      (****DEBUG 1 "~a_nil" spaces)
      (let ((data (node-data node)))
        ;; null?
        (if (null data)
          (****DEBUG 1 "~a_nil <~a>" spaces (print-solutions (solutions node)))
        ;; else if... sub-goal or fact?
        (if (listp data)
          ;; fact?
          (if (eql 'fact (car data))
            (****DEBUG 1 "~a_fact: 'fact <~a>" spaces (print-solutions (solutions node)))
            ;; subgoal:
            (progn
              (****DEBUG 1 "~a_sub-goal: head = ~a <~a>" spaces (car data) (print-solutions (solutions node)))
              (dolist (item (cdr data))      ; cdr skips the subgoal and gets to the literals
                (print-tree-node (concatenate 'string spaces "___") item))))
          ;; rule?
          (progn
            (setf op (car (parameters data)))
            (****DEBUG 1 "~a_rule: op = ~a <~a>" spaces op (print-solutions (solutions node)))
            (dolist (arg (literals data))
              (if (floatp arg)
                (return))
              (print-tree-node (concatenate 'string spaces "___") arg)))))))))

(defun print-explanation (tree)
   (if (not (null tree))
     (progn
       (if (null (third tree))
         ;; 1-ary operator
         (progn
           (format t "~a~%" (car tree))
           (if (eql 'tree-node (type-of (second tree)))
             (print-explanation2 (node-data (second tree)))
             (print-explanation (second tree))))
         ;; 2-ary operator
         (progn
           (if (eql 'tree-node (type-of (second tree)))
             (print-explanation2 (node-data (second tree)))
             (print-explanation (second tree)))
           (format t " ~a~%" (car tree))
           (if (eql 'tree-node (type-of (third tree)))
             (print-explanation2 (node-data (third tree)))
             (print-explanation (third  tree))))))))

(defun print-explanation2 (data)
  (if (eql 'fact (car data))
    (format t "~a" (cadr data))
    (format t "~a" (car data))))

;;; Print the list of solutions into a string
;;;
;;; The escape sequences below set colors.  For a list of ASCII color codes see:
;;;     http://en.wikipedia.org/wiki/ANSI_escape_code#Sequence_elements
;;; -------------------------------------------------------------------
;;; |   0   |  1  |   2   |    3    |   4   |    5    |   6   |   7    |
;;; | Black | Red | Green | Yellow  | Blue  | Magenta | Cyan  | White  |
;;; --------------------------------------------------------------- ---
;;; 30-37 = text color
;;; 40-47 = background color

(if (equal (lisp-implementation-type) "CLISP")
  (progn                               ; for CLISP
    (defparameter *Esc* #\Esc)
    (defparameter *fail-message*    (format nil "~C[36mfail~C[0m" *Esc* *Esc*))
    (defparameter *no-soln-message* (format nil "~C[36mno solution~C[0m" *Esc* *Esc*))
    )
  (progn                               ; for ABCL, but ABCL can't print ASCII colors anyway
    (defparameter *Esc* #\Escape)
    (defparameter *fail-message*    "fail")
    (defparameter *no-soln-message* "no solution")  
    ))

(defun print-solutions (solutions)
  (if (equal 'fail solutions)
    (return-from print-solutions *fail-message*))
  (if (null solutions)
    (return-from print-solutions *no-soln-message*))
  (setf str " ")
  (dolist (solution solutions)
    (setf str
      (concatenate 'string str
           (if (equal (lisp-implementation-type) "CLISP")
             (format nil "~C[36m~a~C[0m " *Esc* (sub solution) *Esc*)    ; print in color
             (format nil "~a " (sub solution)))))
    (setf str
      (concatenate 'string str
           (if (equal (lisp-implementation-type) "CLISP")
             (format nil "~C[31m~a~C[0m " *Esc* (message solution) *Esc*)     ; print in color
             (format nil "~a " (message solution)))))
  )
  str)              ; return the string
