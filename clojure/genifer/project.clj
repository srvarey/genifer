(defproject genifer "1.1.5"
  :description "Genifer logic engine"
  :dependencies [
	[org.clojure/clojure "1.3.0"]
	[org.clojure/math.combinatorics "0.0.2"]
	[cheshire "4.0.0"]
	]
  :main genifer.core
  :test-selectors {
					:forward	:forward
					:backward	:backward
					:unify		:unify
					:subst		:subst
					:narrow		:narrow
					:io			:io
					:all		(fn [_] true)
					:default	(fn [_] true)
				  }
)