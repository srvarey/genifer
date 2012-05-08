;;; Genifer /core.clj
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
;;; ***** Genifer interpreter
;;; 1. Processing Genifer logic terms in Clojure:
;;; 	   A Genifer logic term can be referred by Clojure simply as a list: '(atom atom...).
;;; 	   Processing of Genifer logic terms is performed by Clojure functions processing lists.
;;; 2. Invoking Clojure from Genifer logic:
;;; 	   Just make Clojure function calls with (func ...).
;;; 	   "!" in Genifer input escapes one Clojure term, eg: ![1,2,[3,4]]

(ns genifer.core
	(:require [genifer.forward_chaining :as forward])
	(:require [genifer.backward_chaining :as backward])
	;(:use [genifer.forward_chaining])
	;(:use [genifer.backward_chaining])
	(:use [genifer.unification])
	(:use [genifer.substitution])
	(:use [clojure.string :only [split triml]])
	(:gen-class)
)
(import '(java.io PushbackReader StringReader))
(def pushback-stream (java.io.PushbackReader. (java.io.StringReader. "\n") 1024))

(declare repl process escape-clojure escape-1-clojure tokenize)

(defn -main []
	(repl))

;; Just a simple REPL
;; -- if the input line can be evaluated by Clojure, eval it and print result
;; -- otherwise pass on to Genifer to process
(defn repl []
	;; ASCII rose drawn by Joan Stark, http://www.geocities.com/spunk1111/flowers.htm
	(println "             __                       ") 
	(println "        _   /  |                      ")
	(println "       | \\  \\/_/                      ")
	(println "       \\_\\| / __                      ")
	(println "          \\/_/__\\           .--='/~\\  ")
	(println "   ____,__/__,_____,______)/   /{~}}} ")
	(println "   -,-----,--\\--,-----,---,\\'-' {{~}} ")
	(println "  jgs      __/\\_            '--=.\\}/  ")
	(println "          /_/ |\\\\                     ")
	(println "               \\/                     ")
	(println)
	(loop []
		(print "Genifer> ") (flush)
		(let [	line (read-line)
				clojure-term (read-string line)]	; read line as Clojure term
			(try
				(prn (eval clojure-term))
				(catch RuntimeException e
					(let [message (.getMessage e)]
						(if (and (<= 0 (.indexOf message "Unable to resolve symbol"))
								(Character/isLetterOrDigit (get line 0)))
							(process line)
							(println message))))))
		(recur)))

;; Genifer logic interpreter
;; 0. Scan for "(" and "!", if found, escape with Clojure objects
;; 1. If input is a question, invoke backward-chaining to solve it
;; 2. If input is a statement, store it in KB and invoke forward-chaining
(defn process [line]
	(let [sentence (escape-clojure line)]		; Replace "!" Clojure escapes
		(case (last line)
		\?										; Question?
			;; Call backward-chaining
			(prn
				(backward/solve-goal sentence))
		\.										; Statement?
			;; Call forward-chaining
			(forward/forward-chain sentence)
		;; Statement without "." is fine
			(forward/forward-chain sentence))))

;; Scan for Clojure terms, replace it with the Clojure object
;; "!" is the escape sequence to Clojure, not needed if a term begins with special character 
(defn escape-clojure [line]
	(let [indexes [		(.indexOf line "!")
				   (dec (.indexOf line "["))
				   (dec (.indexOf line "#"))
				   (dec (.indexOf line "("))
				   (dec (.indexOf line "\""))
				   (dec (.indexOf line ":")) ]]
		(if (every? #(< % 0) indexes)
			(tokenize line)
			(escape-1-clojure line (apply min (remove neg? indexes))))))

;; Escape one Clojure object
;; -- the Clojure object will be evaluated before insertion
(defn escape-1-clojure [line index]
	(let [	head	(subs line 0 index)				; minus the "!"
			tail	(subs line (inc index))			; minus the "!"
			length	(count tail)]
		;; Pushback the line into Reader, read it, and see how many chars remain.
		(.unread pushback-stream (char-array length tail))
		(let [	clojure-term	(eval (read pushback-stream))	; eval the object
				num-remaining	(.skip pushback-stream length)]
			(if (== num-remaining 0)
				(concat
					(tokenize head)
					(list clojure-term))
				(concat
					(tokenize head)
					(list clojure-term)
					(escape-clojure
						(triml (subs tail (inc (- length num-remaining))))))))))

(defn tokenize [line]
	(map symbol								; Convert strings to symbols
		(remove #(= % "")
			(split line #"[\s\.\?]"))))		; Split the line on spaces, ".", and "?"