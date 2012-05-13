;;; ***** Narrowing = unification modulo a rewriting system
;;; ==========================================================

(ns genifer.narrowing
	(:require [genifer.unification				:as unify])
	(:require [genifer.unification2				:as unify2])
	(:require [genifer.substitution				:as subst])
	(:require [genifer.knowledge_representation	:as knowledge])
	(:require [clojure.set						:as set])
)

(import '(java.util.concurrent Executors ExecutorCompletionService CancellationException))
(def executor2
	"No harm in sharing one executor for all races."
	(Executors/newCachedThreadPool))

(declare narrow rewrite find-sublist check-prefix fetch-rewrite-rule find-index merge-subs)

;; Narrowing:  unify 2 terms, 'left' and 'right', modulo a rewriting system
;; The algorithm starts with the pair {left =? right} and incrementally "narrows" the equation by applying *one* rewrite rule either to left or right.  Continue such a sequence, until it ends with an equation {left* =? right*} that can be syntactically unified, thus returning success.
;; So this is a search procedure that depends on which rewrite rule we choose to apply at each juncture.  Branching factor may be extremely high due to the size of the rewrite system, and the fact that rewriting can occur at different positions inside a term!
;; In order to optimize, we should 1) search the rewrite system by the terms-to-be-unified at current positions, 2) allow rewriting only in an oriented direction.
;; 1. See if unify(left, right) succeeds, if so return solution
;; 2. For each of 'left' and 'right':
;; 3.	For each position within the term:
;; 4.		Find rewrite rules that may apply
;; 5.		Try unify sub-term with left side of rewrite rule
;; 6.		If unifiable, apply sub to both left and right, then apply rewrite rule
;; 7.		Spawn sub-processes to continue search (recurse)
;; OUTPUT:  a list of compound subs
(defn narrow
([left right]											; call with default arguments
	(narrow left right ())
)
([left right subs]								; full set of arguments
	(printf "\n<><><><><><><><><><><><><><><><><><><><><><>\n")
	(println "entry: left right = " left ", " right)
	(let [subs2 (unify/unify left right)]
	(println "plain unify: left right, subs2 = " left ", " right ", " subs2)
		(if (not (false? subs2))
			(merge-subs subs2 subs)				; Success, return sub + subs
			;; Else: try rewriting -- don't wanna indent

	;; Now a series of nested for-loops that collects a list of futures
	(let [	;comp-service (ExecutorCompletionService. executor2)
			futures (doall (remove false?
		(for [	;; Pick a term to try;  term2 will be the 'other' term, ie dummy
				[term term2]	[[left right] [right left]]
				;; Fetch rules with indexes
				:let [rules+	(fetch-rewrite-rule term)]
				;; Each rule+ has a list of indexes into its head
				rule+			rules+
				;; Each rule has yet to be standardized apart, hence the "_"
				:let [[indexes rule_] rule+]
				;; For each index...
				index			indexes
				;; Try unify (special versions, see "unification2")
				:let [	rule	(unify/standardize-apart rule_)
						index2	(.indexOf term (nth (first rule) index))
						;; term-R = rest of term after index2, inclusive
						term-R	(nthrest term index2)
						;; term-L = *reverse* of term before index2, exclusive
						term-L	(reverse (take index2 term))
						;; Define head-L, head-R similarly
						head	(first rule)			; head of rule
						head-R	(nthrest head index)
						head-L	(reverse (take index head))
						;; Try unify
						subs-R	(unify2/unify-R head-R term-R)
						subs-L	(unify2/unify-L head-L term-L) ]]

(do ; ***** DEBUG
(println "mid way: term, term2 = " term ", " term2)
(println "mid way: trying rule ==> " rule)
;(println "term-R&L==> " term-R " & " term-L)
;(println "head-R&L==> " head-R " & " head-L)
;(println "mid way: left, right = " left ", " right)
;(println "subs-R&L==> " subs-R " & " subs-L)
;(println "types==> " (type subs-R) (type subs-L))

			(if (not (and (seq? subs-R) (seq? subs-L)))
				(do (println "shit is false") false)
				(doall (for [	sub-R	subs-R		; try all combinations
								sub-L	subs-L ]
(do ; ***** DEBUG
;(println " sub-R&L==> " sub-R " & " sub-L)

					(let [	sub	(set/union sub-R sub-L)]
						(println "post-unify-LR: sub = " sub)
						(if (subst/compatible? sub)
						(let [	;; apply sub to term and dummy
								term*		(subst/substitute sub term)
								term2*		(subst/substitute sub term2)
								rule-head	(subst/substitute sub (first  rule))
								rule-body	(subst/substitute sub (second rule))
								; rewrite the term
								term**	(rewrite rule-head rule-body term*) ]
					; recurse and add the new sub to subs collection
(do ; ***** DEBUG
(print "post-rewrite: term**==> ") (prn term**)
(print "post-rewrite: term2*==> ") (prn term2*)
(println)
					;(.submit comp-service #(narrow term** term2*
					(narrow term** term2*
						(merge-subs (list sub) subs))))))
					)
					)))
			)
 ))) ]
		(println "final: #futures = " (count futures))
		;(println "futures = " futures)
		;(println "type is " (type futures))
		(if (empty? futures)
			false
		;; Get the first solution that is not false -- Note that the list of futures may be non-empty but the futures could all return false
			(let [solution (doall
					(first (drop-while false?
						(take (count futures)
							futures))))]
							; (repeatedly #(.get (.take comp-service)))))))]
				(if (nil? solution)
					false
					solution))))))))

;; Merge (via AND semantics) 2 lists of compound subs
(defn merge-subs [subs1 subs2]
	(if (empty? subs2)
		subs1
		(for [	sub1 subs1
				sub2 subs2 ]
			(set/union sub1 sub2))))

;; Search term for old, replace with new
;; -- Cannot use substitute because "old" can consist of multiple atoms.
;; -- A problem arises when rewrite is possible at multiple positions;  Now we assume we can rewrite at the leftmost position and the search will find the other positions later.
(defn rewrite [old new term]
	(println "rewrite old new term ==> " old "--->" new " @ " term)
	(let [index (find-sublist old term)]
		(concat
			(take index term)
			new
			(nthrest term (+ index (count old))))))

;; Find position of sublist in list
(defn find-sublist [sublist list]
	(if (empty? list)
		false
		(let [position	(.indexOf list (first sublist))]
			(if (check-prefix sublist (nthrest list position))
				; if yes return position, end-position
				position
				(find-sublist sublist (nthrest list (inc position)))))))

;; Check if sublist is a prefix of list
(defn check-prefix [sublist list]
	(if (empty? list)
		(if (empty? sublist)
			true
			false)
		(if (empty? sublist)
			true
			(if (= (first sublist) (first list))
				(check-prefix (rest sublist) (rest list))
				false))))

;; Select rules that have a common atom with term, together with index into rule head
;; -- Note:  Rewriting will occur only in the left => right direction.
(defn fetch-rewrite-rule [term]
	(remove false?
		(map #(let [index (find-index (first %) term)]	; (first %) = left side of rule
					(if (false? index)
						false
						[index %]))			; return index and the rule
			knowledge/rewrite-sys)))

;; Find index of any key inside term
;; OUTPUT:  integer or false
(defn find-index [term keys]
	(let [result (remove #(= % -1)
					(for [key keys]
						(.indexOf term key)))]
		(if (empty? result)
			false
			result)))
