;;; ***** Substitution management
;;; ==========================================================

(ns genifer.substitution)
(declare substitute substitute-atomic compatible? atomic-compatible?)

;; Apply a compound substitution to a term
;; INPUT: subs = a single compound sub = list of atomic subs
;;		 term, as list
;; OUTPUT: new term
(defn substitute [subs term]
	(reduce #(substitute-atomic %2 %1) term subs))

;; Make an atomic substitution, returns new term
(defn substitute-atomic [sub term]
	(let [	old (first sub)
			new (rest sub)]
		(reduce #(concat %1				; Reduce with concatenations
					(if (= %2 old)		; If match, replace old with new
						new
						(list %2)))
				() term)))

;; Test a list of atomic subs against each other
;; Returns:  true if compatible, false if not
;; -- For example, {john /X} and {joe /X} are incompatible
(defn compatible? [subs-list]
	(every? #(= % true)
		(for [sub1 subs-list		; This generates all possible pairs
			  sub2 subs-list]
			(atomic-compatible? sub1 sub2))))

;; Check if 2 atomic subs are compatible, returns true if OK
(defn atomic-compatible? [sub1 sub2]
	(or
		;; Same variables?  If not that's good
		(not= (first sub1) (first sub2))
		;; Otherwise, if substituents are the same that's OK too
		(= (rest sub1) (rest sub2)))
		;; Another possibility is if sub1 unifies with sub2 -- To-do
)
