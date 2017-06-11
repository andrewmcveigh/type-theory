(ns type
  (:require [clojure.set :as set]))

(defprotocol Free
  (free [_]))

(defprotocol Show
  (show [_]))

(defmacro print-show [& types]
  `(do
     ~@(map (fn [t]
              `(defmethod print-method ~t [x# w#]
                 (.write w# (show x#))))
            types)))

(defprotocol Substitutable
  (substitute [x substitution]))


(defrecord TVar [value]
  Show
  (show [_] (format "TVar %s" (pr-str value)))
  Substitutable
  (substitute [x substitution]
    (get substitution value x))
  Free
  (free [_] #{value}))

(defrecord TConst [value]
  Show
  (show [_] (format "TConst %s" (pr-str value)))
  Substitutable
  (substitute [x _] x)
  Free
  (free [_] #{}))

(defrecord TArrow [t1 t2]
  Show
  (show [_] (format "(%s -> %s)" (pr-str t1) (pr-str t2)))
  Substitutable
  (substitute [_ substitution]
    (TArrow. (substitute t1 substitution)
             (substitute t2 substitution)))
  Free
  (free [_] (set/union (free t1) (free t2))))

(def t-int?  (TConst. 'int?))
(def t-bool? (TConst. 'boolean?))
