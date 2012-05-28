(ns genifer.test.core
	(:use [clojure.test])
	(:require [genifer.core])
	(:require [genifer.forward_chaining :as forward])
	(:require [genifer.backward_chaining :as backward])
	(:require [genifer.narrowing :as narrow])
	(:require [genifer.unification :as unify])
	(:require [genifer.substitution :as subst])
	(:require [genifer.io :as io])
)

(deftest ^:io test_io
	(printf "=============================== I/O ==============================\n")
	(println "Y=1 M=0 C=2 A=3, expect 3*2*0*1")
	(println
		(io/build-formula '((0 2) (1 0) (2 3)) ()))
	(println)

	(println
		(io/formularize "2,0,3," "M,Y,C,A,"))
	(println)
	
	(prn "This is a very long test")
	(println
		(io/formularize "1,2,3,4,5,-1," "this,is,a,very,long,test"))
	(println)

	(prn "John walks slowly to church")
	(println
		(io/formularize "-1,0,1,1,3" "john,walks,slowly,to,church"))
	(println)

	(prn "They stab it with their steely knives")
	(println
		(io/formularize "-1,0,1,1,6,6,3," "they,stab,it,with,their,steely,knives"))
	(println)

	(prn "The best way to predict the future is to invent it")
	(println
		(io/formularize "1,2,-1,2,3,6,4,2,7,8,9," "the,best,way,to,predict,the,future,is,to,invent,it"))
	(println)
)

(deftest ^:narrow test_narrow ; Narrowing
	(printf "============================ NARROWING =========================\n")

	(printf "\n**** '(chalk) '(cheese) ==> \n\t")
	(println
		(narrow/narrow '(chalk) '(cheese)))

	(printf "\n**** '(bob eats cheese) '(bob eats cheese) ==> \n\t")
	(println
		(narrow/narrow '(bob eats cheese) '(bob eats cheese)))

	(printf "\n**** '(love) '(cheese) ==> \n\t")
	(println
		(narrow/narrow '(love) '(cheese)))

	(printf "\n**** '(i love cheese) '(i love cheese) ==> \n\t")
	(println
		(narrow/narrow '(i love cheese) '(i love cheese)))

	(printf "\n**** '(love cheese) '(like cheese) ==> \n\t")
	(println
		(narrow/narrow '(love cheese) '(like cheese)))

	(printf "\n**** '(i love cheese) '(i like cheese) ==> \n\t")
	(println
		(narrow/narrow '(i love cheese) '(i like cheese)))

	(printf "\n**** '(i love cheese) '(X like Y) ==> \n\t")
	(println
		(narrow/narrow '(i love cheese) '(X like Y)))

	(printf "\n**** '(mark is jealous of kent) '(X envies Y) ==> \n\t")
	(println "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>" 
		(narrow/narrow '(mark is jealous of kent) '(X envies Y)))
	true
)

(deftest ^:forward test_forward ; Forward-chaining
	;(is (= 1 1) "Testing forward-chaining...")
	;(satisfy-rule '[[X and Y are happy] [X loves Y] [Y loves X]])
	;(println "Working memory: " @work-mem)
	(println "Testing forward: "
		(forward/forward-chain '(john hates mary)))
	true
)

(deftest ^:backward test_backward ; Backward-chaining
	(printf "\n**** match facts: john loves mary ==> \n\t")
	(println
		(backward/match-facts '(john loves mary)))

	(printf "\n**** john loves mary ==> \n\t")
	(println
		(backward/solve-goal '(john loves mary)))

	(printf "\n**** X loves mary ==> \n\t")
	(println
		(backward/solve-goal '(X loves mary)))

	(printf "\n**** match rules: joe is sad ==> \n\t")
	(println
		(backward/match-rules '(joe is sad)))

	(printf "\n**** joe is sad ==> \n\t")
	(println
		(backward/solve-goal '(joe is sad)))

	(printf "\n**** U is sad ==> \n\t")
	(println
		(backward/solve-goal '(U is sad)))

	(printf "\n**** match rules: Q and R is happy ==> \n\t")
	(println
		(backward/match-rules '(Q and R are happy)))

	(printf "\n**** solve rule: Q loves R and R loves Q ==> \n\t")
	(println
		(backward/solve-rule '((Q loves R) (R loves Q))))

	(printf "\n**** Q and R is happy ==> \n\t")
	(println
		(backward/solve-goal '(Q and R are happy)))

	(printf "\n**** genifer cries ==> \n\t")
	(println
		(backward/solve-goal '(genifer cries)))

	true
)

(deftest ^:subst test_subst ; Substitution
	(println "compatible? ==> "
		(subst/compatible? '((R paul) (Q ann) (R paul) (Q ann))))
	(println "compatible? ==> "
		(subst/compatible? '((R paul) (Q ann) (R ron) (Q ann))))
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
	;15
	(X loves Y)				(Z loves mary obsessively)
		( #{(X Z) (Y mary obsessively)}
		  #{(Z X) (Y mary obsessively)}
		  #{(Z X loves) (Y loves mary obsessively)} )
	;16
	(X loves Y)				(john Z mary)
		( #{(X john) (Z loves) (Y mary)} )
	;17
	(X loves Y)				(john loves Z)
		( #{(X john) (Y Z)}
		  #{(X john) (Z Y)}
		  #{(X john loves) (Z loves Y)}
		)
	;18  This example shows that subs on left and right may be incompatible -- consistency is not checked by unify.
	(X Y Z)					(Z mary)
		( #{(X mary) (Y) (Z mary)} __et_cetera__ )
	; 19
	(13 is "Thirteen !!!")		(X is Y)
		( #{(X 13) (Y "Thirteen !!!")} )
	))

(deftest ^:unify test_unify
	(loop [test unify-tests
		   count 1]
		(let [t1 (first test)
			  t2 (second test)
			  answer (nth test 2)]
			(println count ".")
			(println "Expected: " answer)
			(println "     ===> " (unify/unify t1 t2))
			(if (empty? (rest (rest (rest test))))
				true
				(recur (rest (rest (rest test)))
					   (+ count 1))))))