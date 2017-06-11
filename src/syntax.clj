(ns syntax
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [clojure.set :as set]
   [type :refer [print-show Show]]))

(defrecord Var [value]
  Show
  (show [_] (pr-str value)))

(defrecord App [value]
  Show
  (show [_]
    (str \( (string/join " " (map pr-str value)) \))))

(defrecord Abs [value]
  Show
  (show [_] (pr-str value)))

(defrecord Let [value]
  Show
  (show [_] (pr-str value)))

(print-show Var App Abs Let)


;;; Parsing

(s/def ::exp
  (s/or ::sub ::sub
        ::var ::var
        ::app ::app
        ::abs ::abs
        ::let ::let))

(s/def ::var simple-symbol?)

(s/def ::app
  (s/and seq? (s/cat :op ::exp :args (s/* ::exp))))

(s/def ::abs
  (s/cat :fn #{'fn} :arg (s/and vector? (s/cat :arg ::var)) :exp ::exp))

(s/def ::let
  (s/cat :let #{'let}
         :bin (s/and vector? (s/cat :var ::var :exp ::exp))
         :exp ::exp))

(s/def ::subc
  (s/or :id  (s/and vector? empty?)
        :sub (s/and vector? (s/cat :term ::exp :/ #{'/} :var ::exp))))

(s/def ::sub
  (s/or :subs (s/cat :subs (s/* (s/cat :subc ::subc :comp #{'◦})) :sub ::subc)
        :subc ::subc))

(defn parse-ast [[t x]]
  (case t
    ::var (Var. x)
    ::app (App. (vec (cons (parse-ast (:op x))
                           (map parse-ast (:args x)))))
    ::sub (let [[t' x'] x
                ->sub (fn [[t x]]
                        (case t
                          :id  idsub
                          :sub (Sub. [[(parse-ast (:term x))
                                       (parse-ast (:var x))]]
                                     [[(parse-ast (:term x))
                                       (parse-ast (:var x))]])))]
            (case t'
              :subs (apply compose
                           (conj (mapv (comp ->sub :subc) (:subs x'))
                                 (->sub (:sub x'))))
              :subc (->sub x')))))

(defn parse [x]
  (parse-ast (s/conform ::exp x)))
