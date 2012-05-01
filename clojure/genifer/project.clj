(defproject genifer "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [
	[org.clojure/clojure "1.3.0"]
	[org.clojure/math.combinatorics "0.0.2"]
	]
  :main genifer.core
  :test-selectors {:default (complement :forward :backward)
                   :forward :forward
				   :backward :backward
                   :all (fn [_] true)}
)