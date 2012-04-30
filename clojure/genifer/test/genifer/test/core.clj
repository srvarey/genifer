(ns genifer.test.core
  (:use [genifer.core])
  (:use [clojure.test])
  (:use [genifer.forward_chaining])
  (:use [genifer.unification])
)

(deftest ^:forward test_forward ; Forward-chaining
	;(is (= 1 1) "Testing forward-chaining...")
	;(satisfy-rule '[[X and Y are happy] [X loves Y] [Y loves X]])
	;(println "Working memory: " @work-mem)
	(println "Testing forward: "
		(forward '(john hates mary)))
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

(deftest test_unify ; Unify
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