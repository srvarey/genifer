(defproject genifer "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [
	[org.clojure/clojure "1.3.0"]
	[org.clojure/math.combinatorics "0.0.2"]
	]
  :main genifer.core
  :test-selectors {	:default :unify
					:forward :forward
					:backward :backward
					:unify :unify
					:subst :subst
					:all (fn [_] true)
				  }
)