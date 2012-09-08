;;; Hierarchical clustering
;;; ==========================================================

(ns genifer.io
	(:require	[genifer.knowledge_representation :as knowledge]
))
(declare tree-index concept-distance product-distance sum-distance formula-distance)

;; Check if the compound c is a valid concept
;; -- yes, if there is an entry in the ontology for compound concept c
;; -- or, if abduction produces a valid interpretation of compound c
(defn concept? [c]
)

;; ***** distance between 2 concepts, each concept is either an atom or an irreducible compound
;; -- Suppose all defined concepts belong to a classification tree, then d(c1,c2) can be defined as the absolute difference between their tree indexes
(defn concept-distance [c1 c2]
	(Math/abs (tree-index c1) (tree-index c2)))

;; ***** distance between 2 products
;; -- In Genifer's logical form, the natural-language subject is the last atom in a product, thus it should be given the heaviest weight.  Secondly, the grammatical structure of terms should be taken into account, so we can compare like atoms with each other.  Another way to achieve meaningful comparison is to find the minimum of the distances of matching atoms (and preserving the order of composition).  We use the latter method here.
;; INPUT:  2 lists

;; Find all ways to match atoms from one list to another
;; At each juncture, the shorter list p1 either consumes or not consumes an atom from p2
;; INPUT:  p1 = shorter list, p2 = longer
;; OUTPUT:  a list of distances
(defn product-distance [p1 p2 results]
	(cond
	(empty? p1)
		;; Rest of atoms in p2 should all map to 0
		(list (apply +
			(for [c2 p2]
				(concept-distance c2 0))))
	(empty? p2)
		;; Rest of atoms in p1 should all map to 0
		(list (apply +
			(for [c1 p1]
				(concept-distance c1 0))))
	:else
		;; We can take 1,2,3,... atom(s) from p1 to form a concept c1, ditto for c2 from p2
		;; If p1 (or p2) is shorter than 3, take fewer atoms
		(apply concat (remove nil?
			(for [	n1 (range 1 (inc (min 3 (count p1))))
					n2 (range 1 (inc (min 3 (count p2)))) ]
				(let [	c1	(take n1 p1)
						c2	(take n2 p2) ]
					(if (and (concept? c1) (concept? c2))
						(product-distance (nthrest p1 n1) (nthrest p2 n2)
							(map #(+ % (concept-distance c1 c2)) results)))))))))
;; each call should return a list... what for?

;; ***** distance between 2 sums
;; -- d(s1,s2) should be the minimum of distances between matching factors (ie, products).  If a factor has a null counterpart, the null formula can be assumed to have index 0.0.0..., but we can also take the distance to be the next closest match (which is easier to program).
;; INPUT:  each s can be a set of lists or a list
(defn sum-distance [s1 s2]
			;; if s is a list (singleton), wrap a set around it
	(let [	s1*		(if (list? s1) #{s1} s1)
			s2*		(if (list? s2) #{s2} s2)
			shorter	(if (> (count s1*) (count s2*)) s2* s1*)
			longer	(if (> (count s1*) (count s2*)) s1* s2*)
			;; Take the smallest n matches
			matches	(take (count longer) (sort
						(for [p1 shorter p2 longer]
							(product-distance p1 p2)))) ]
		(apply + matches)))

;; ****** distance between 2 formulas
;; A formula is of the form:  term /\ term /\ ... -> term /\ term /\ ... , where each term is a sum, and the premise may be null, in which case it is a fact.
;; We use the same trick:  find the minimum distances between the 2 heads and the 2 bodies, add them up.
;; INPUT:  each formula = (head body)
(defn formula-distance [f1 f2]
	(let [	head1		(first  f1)
			head2		(first  f2)
			body1		(second f1)
			body2		(second f2)
			shorterH	(if (> (count head1) (count head2)) head2 head1)
			longerH		(if (> (count head1) (count head2)) head1 head2)
			shorterB	(if (> (count body1) (count body2)) body2 body1)
			longerB		(if (> (count body1) (count body2)) body1 body2)
			matchesH	(take (count longerH) (sort
							(for [s1 shorterH s2 longerH]
								(sum-distance s1 s2))))
			matchesB	(take (count longerB) (sort
							(for [s1 shorterB s2 longerB]
								(sum-distance s1 s2)))) ]
		(+	(apply + matchesH)
			(apply + matchesB))))
