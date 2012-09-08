;;; Genifer interpreter
;;; ==========================================================
;;; 1. Processing Genifer logic terms in Clojure:
;;; 	   A Genifer logic term can be referred by Clojure simply as a list: '(atom atom...).
;;; 	   Processing of Genifer logic terms is performed by Clojure functions processing lists.
;;; 2. Invoking Clojure from Genifer logic:
;;; 	   Just make Clojure function calls with (func ...).
;;; 	   "!" in Genifer input escapes one Clojure term, eg: ![1,2,[3,4]]

(ns genifer.core
	(:require	[genifer.forward_chaining :as forward]
				[genifer.backward_chaining :as backward]
				[clojure.main :as clj]
				[genifer.chat :as chat]
				[clojure.string :as string] )
	(:gen-class)
		; :name genifer.core
		; :methods [#^{:static true} [repl1 [String] String]]
)
(import '(java.io PushbackReader StringReader))
(def pushback-stream (java.io.PushbackReader. (java.io.StringReader. "\n") 1024))

(declare repl repl1 process escape-clojure escape-1-clojure tokenize help)

(defn -main []
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
	(println "               \\/                     ")
	(println)
	(println "Type (help) for help, Ctrl-C to exit.")
	(println)
	(loop []
		(print "Genifer> ") (flush)
		(printf (repl1 (read-line)))
		(println)
		(recur)))

;; Evaluate Clojure expression in Genifer namespace
(defn eval-in-ns [exp]
	(binding [*ns* (the-ns 'genifer.core)]
		(eval exp)))

;; Just a simple REPL
;; -- if the input line can be evaluated by Clojure, eval it and print result
;; -- otherwise pass on to Genifer to process
(defn repl []
	(loop []
		(print "Genifer> ") (flush)
		(let [	line (read-line)
				clojure-term (read-string line)]	; read line as Clojure term
			(try
				(prn (eval-in-ns clojure-term))
				(catch RuntimeException e
					(let [message (.getMessage e)]
						(if (and (<= 0 (.indexOf message "Unable to resolve symbol"))
								(Character/isLetterOrDigit (get line 0)))
							(process line)
							(println message))))))
		(recur)))

;; **** 1-line REPL
;; OUTPUT:  a string, either error message or results
(defn repl1 [line]
	(let [	; line (read-line)
			clojure-term (read-string line)]	; read line as Clojure term
		(try
			(prn-str (eval-in-ns clojure-term))
			(catch RuntimeException e
				(let [message (.getMessage e)]
					(if (and (<= 0 (.indexOf message "Unable to resolve symbol"))
							(Character/isLetterOrDigit (get line 0)))
						(process line)
						(println-str message)))))))

;; Genifer logic interpreter
;; 0. Scan for "(" and "!", if found, escape with Clojure objects
;; 1. If input is a question, invoke backward-chaining to solve it
;; 2. If input is a statement, store it in KB and invoke forward-chaining
(defn process [line]
	(let [sentence (escape-clojure line)]		; Replace "!" Clojure escapes
		(case (last line)
		\?										; Question?
			;; Call backward-chaining
			(prn-str
				(backward/solve-goal sentence))
		\.										; Statement?
			;; Call forward-chaining
			(do
				(forward/forward-chain sentence 4)
				"Forward-chaining completed.\n")
		;; Statement without "." is fine
			(do
				(forward/forward-chain sentence 4)
				"Forward-chaining completed.\n"))))

;; Scan for Clojure terms, replace it with the Clojure object
;; "!" is the escape sequence to Clojure, not needed if a term begins with special character 
(defn escape-clojure [line]
	(let [indexes [		(.indexOf line "!")
				   (dec (.indexOf line "0"))
				   (dec (.indexOf line "1"))
				   (dec (.indexOf line "2"))
				   (dec (.indexOf line "3"))
				   (dec (.indexOf line "4"))
				   (dec (.indexOf line "5"))
				   (dec (.indexOf line "6"))
				   (dec (.indexOf line "7"))
				   (dec (.indexOf line "8"))
				   (dec (.indexOf line "9"))
				   (dec (.indexOf line "["))
				   (dec (.indexOf line "'"))
				   (dec (.indexOf line "#"))
				   (dec (.indexOf line "("))
				   (dec (.indexOf line "\""))
				   (dec (.indexOf line ":")) ]]
		(if (every? #(< % 0) indexes)
			(tokenize line)
			;; Escape the first occurrence of clojure item
			(escape-1-clojure line (apply min (remove neg? indexes))))))

; (defmacro eval-data[ & body ]
	; (defonce this-current-context (get-thread-bindings) )
	; `(do (push-thread-bindings this-current-context)
	; (try (eval ~@body) (finally (pop-thread-bindings)))))

;; Escape one Clojure object
;; -- the Clojure object will be evaluated before insertion
(defn escape-1-clojure [line index]
	(let [	head	(subs line 0 index)				; minus the "!"
			tail	(subs line (inc index))			; minus the "!"
			length	(count tail)]
		;; Pushback the line into Reader, read it, and see how many chars remain.
		(.unread pushback-stream (char-array length tail))
		(let [	;; eval the object:
				clojure-term	(eval-in-ns (read pushback-stream))
				num-remaining	(.skip pushback-stream length)]
			(if (== num-remaining 0)
				(concat
					(tokenize head)
					(list clojure-term))
				(concat
					(tokenize head)
					(list clojure-term)
					(escape-clojure
						(string/triml (subs tail (inc (- length num-remaining))))))))))

(defn tokenize [line]
	(map symbol								; Convert strings to symbols
		(remove #(= % "")
			(string/split line #"[\s\.\?]"))))	; Split the line on spaces, ".", and "?"

(defn help []
	(println "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><>")
	(println "You can evaluate any Clojure expression, eg:")
	(println "    (+ 1 2)")
	(println "    (defn triple [x] (+ x x x))")
	(println "    (triple 7)")
	(println "    [1,2,3,4]")
	(println "Simple Genifer logic statements (fullstop is optional):")
	(println "    john loves mary.")
	(println "    john loves mary obsessively")
	(println "Simple statements will be added to KB, then forward-chaining will be called.")
	(println "Questions end with '?', eg:")
	(println "    john loves mary?")
	(println "    mary is happy?")
	(println "    X is sad?")
	(println "(Variable names begin with uppercase)")
	(println "Questions invoke backward-chaining")
	(println "Clojure objects can be included in Genifer logic, eg:")
	(println "    john loves number (+ 10 3)")
	(println "    \"Clark Kent\" is string")
	(println "    [1,2,3,4] is list")
	(println "'!' escapes a Clojure term explicitly, eg:")
	(println "    namespace is !*ns*")
	(println "More functions (such as learning) will be added later.")
	(println "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><>")
	(println)
	"Output has directed to STDOUT.\n")

;; ==============================================================
;; This may be a simpler way to invoke the Clojure REPL, will explore later when I have time

;(defn repl2 []
;	(binding [*ns* (the-ns 'genifer.core)]
;		(clojure.main/repl :caught process-error)))
;
;(defn process-error [e]
;	(println "Exception thrown"))
