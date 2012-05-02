(ns genifer.test.core
  (:use [genifer.core])
  (:use [clojure.test])
  (:use [genifer.forward_chaining])
  (:use [genifer.backward_chaining])
  (:use [genifer.unification])
  (:use [genifer.substitution])
)

(deftest ^:forward test_forward ; Forward-chaining
	;(is (= 1 1) "Testing forward-chaining...")
	;(satisfy-rule '[[X and Y are happy] [X loves Y] [Y loves X]])
	;(println "Working memory: " @work-mem)
	(println "Testing forward: "
		(forward '(john hates mary)))
	true
)

(deftest ^:backward test_backward ; Backward-chaining
	(printf "\n**** match facts: john loves mary ==> \n\t")
	(println
		(match-facts '(john loves mary)))
	(printf "\n**** john loves mary ==> \n\t")
	(println
		(solve-goal '(john loves mary)))
	(printf "\n**** X loves mary ==> \n\t")
	(println
		(solve-goal '(X loves mary)))
	(printf "\n**** match rules: joe is sad ==> \n\t")
	(println
		(match-rules '(joe is sad)))
	(printf "\n**** joe is sad ==> \n\t")
	(println
		(solve-goal '(joe is sad)))
	(printf "\n**** U is sad ==> \n\t")
	(println
		(solve-goal '(U is sad)))
	(printf "\n**** match rules: Q and R is happy ==> \n\t")
	(println
		(match-rules '(Q and R are happy)))
	(printf "\n**** solve rule: Q loves R and R loves Q ==> \n\t")
	(println
		(solve-rule '((Q loves R) (R loves Q))))
	(printf "\n**** Q and R is happy ==> \n\t")
	(println
		(solve-goal '(Q and R are happy)))
	true
)

(deftest ^:subst test_subst ; Substitution
	(println "compatible? ==> "
		(compatible? '((R paul) (Q ann) (R paul) (Q ann))))
	(println "compatible? ==> "
		(compatible? '((R paul) (Q ann) (R ron) (Q ann))))
	true
)

(def unify-tests '(
	;1
	(john loves mary)   	(john loves mary)
		(#{()})
	;2
	(john loves mary)   	(john hates mary)
		false
	;3
	(john loves mary X)		(john loves mary obsessively)
		(#{(X obsessively)})
	;4
	(john loves X)      	(john loves mary obsessively)
		(#{(X mary obsessively)})
	;5
	(john X)            	(john loves mary obsessively)
		(#{(X loves mary obsessively)})
	;6
	(john X possessively)	(john loves mary obsessively)
		false
	;7
	(john X obsessively)	(john loves mary obsessively)
		(#{(X loves mary)})
	;8
	(X)						(love)
		(#{(X love)})
	;9
	(love)                 	(Y)
		(#{(Y love)})
	;10
	()						(love)
		false
	;11
	(X loves mary)			(john loves mary)
		(#{(X john)})
	;12
	(X loves Y)				(john loves mary)
		(#{(X john) (Y mary)})
	;13
	(X loves Y Z)			(john loves mary)
		( #{(X john) (Y mary) (Z)}
	      #{(X john) (Y) (Z mary)} )
	;14
	(X loves Y Z)			(john loves mary obsessively)
		( #{(X john) (Y mary) (Z obsessively)}
	      #{(X john) (Y mary obsessively) (Z)}
	      #{(X john) (Y) (Z mary obsessively)} )
	))

(deftest ^:unify test_unify
	(loop [test unify-tests
		   count 1]
		(let [t1 (first test)
			  t2 (second test)
			  answer (nth test 2)]
			(println count ".")
			(println "Expected: " answer)
			(println "     ===> " (unify t1 t2))
			(if (empty? (rest (rest (rest test))))
				true
				(recur (rest (rest (rest test)))
					   (+ count 1))))))