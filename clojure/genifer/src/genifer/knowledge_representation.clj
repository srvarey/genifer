;;; Definitions of logic, knowledge representation, and memory systems
;;; ==========================================================

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

;; So a formula would be (a b c ...)
;; A rule would be ((a1 b1 c1...) (a2 b2 c2...) (a3 b3 c3...) ...)

;;;; ========================= KB =============================
;; KB (knowledge-base)
;; -- perhaps should be implemented as a hash-table, ie, a hash-map with vector values
;; (get {:a 1 :a 2 :b 3} :a) ?
;; (merge-with vector {:a 1 :b 2} {:a 1})
;; ==> {:a [1 1], :b 2}
;; (def rules (hash-map 'goal '[premises]))

(def rules '(
;;	Conclusion    		    <=== 	Premises .....
	;((X and Y are happy)	(X loves Y) (Y loves X))
	;((W is sad)				(Z hates W))
	((X cries)				(X is sad))

	((S)					(S is-a sentence))
	((NP VP is-a sentence)	(NP is-a noun-phrase) (VP is-a verb-phrase)
								(text X1 NP VP X2))
	((V NP is-a verb-phrase) (V is-a verb) (NP is-a noun-phrase)
								(text X1 V NP X2))
	((N is-a noun-phrase)	(N is-a noun))
	((X is-a verb)			(lexeme-loves X))
	((X is-a noun)			(lexeme-John X))
	((X is-a noun)			(lexeme-Mary X))
))

;; ***** Working memory
;; -- just a list of facts
;; -- use clojure "agent" model because transactions can be asynchronous & uncoordinated
(def working-mem (agent
(list
	;'(john loves mary)
	;'(john loves jane)
	;'(pete loves ann)
	;'(paul loves ann)
	;'(ann  loves paul)
	;'(john hates joe)
	;'(superman can fly)
	;'("Clark Kent" is superman)
	;'(roses are red)
	'(13 is prime)
)))

;; ***** Rewrite system
;; -- The system is oriented, following these general rules:
;;		* from uncommon to common (frequent)
;;		* from complex to simpler
;;		* from longer to shorter

;; -- We can use a hash-map with the first element of "left" as key.  This may be done later.
(def rewrite-sys '(
;;	Left			===>		Right
	((very X)			(X))
	((unhappy)			(not happy))
	((love)				(like))
	((envy)				(jealousy))
	((sad)				(unhappy))
	((clark kent)		(superman))
	((yan king yin)		(yky))
	((is jealous of Y), (envies Y))
	((gives Y the creeps), (disgusts Y))
))

;; ***** Dump memory
(defn dump-mem []
	(doseq [item @working-mem]
		(prn item)
		(println)))
