(def dependencies
  '[[org.clojure/clojure "1.9.0-alpha17"]])

(def test-dependencies
  '[[org.clojure/test.check "0.9.0"]])

(set-env! :dependencies (vec (concat dependencies test-dependencies))
          :source-paths #{"src"})

(defn cider? []
  (get (ns-publics 'boot.user) 'cider))

(replace-task!
 [r repl] (comp ((or (cider?) (constantly identity))) r))
