(defproject genifer "1.0.5"
  :description "Genifer logic engine"
  :dependencies [
	[org.clojure/clojure "1.3.0"]
	[org.clojure/math.combinatorics "0.0.2"]
	]
  :main genifer.core
  :test-selectors {
					:default (fn [_] true)
					:forward :forward
					:backward :backward
					:unify :unify
					:subst :subst
					:narrow :narrow
					:all (fn [_] true)
				  }
)