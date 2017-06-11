(ns algorithm-w
  (:refer-clojure :exclude [extend])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [clojure.set :as set]
   [type :refer :all]))

;;; Context / Type Schemes

(deftype Env [value]
  clojure.lang.Associative
  (assoc [_ key val]
    (Env. (assoc value key val)))
  clojure.lang.ILookup
  (valAt [_ k]
    (.valAt value k nil))
  (valAt [_ k not-found]
    (.valAt value k not-found))
  clojure.lang.IPersistentMap
  (without [_ key]
    (Env. (dissoc value key)))
  Show
  (show [_] (str \Γ (pr-str value)))
  Substitutable
  (substitute [_ substitution]
    (->> value
         (map (fn [[k v]] [k (substitute v substitution)]))
         (into {})
         (Env.)))
  Free
  (free [_]
    (set (map free (vals value)))))

(def fresh-vars
  (for [a (map char (range 97 123))
        i (rest (range))]
    (symbol (str a i))))

(defn fresh [state]
  [(nth fresh-vars (:next state))
   (update state :next inc)])

(defn map-m [f state xs]
  (loop [state state
         xs    xs
         ys    []]
    (if (seq xs)
      (let [[a s] (f state (first xs))]
        (recur s (rest xs) (conj ys a)))
      [ys state])))

(defrecord Scheme [vars t]
  Substitutable
  (substitute [_ substitution]
    (Scheme. vars (substitute t (apply dissoc substitution vars))))
  Free
  (free [_]
    (set/difference (free t) (set vars))))


;;; Substitution

(declare sub)

(deftype Sub [orig value]
  clojure.lang.IHashEq
  (equals [_ other]
    (and (instance? Sub other)
         (= orig (.-orig other))
         (= value (.-value other))))
  clojure.lang.ILookup
  (valAt [x k]
    (.valAt x k nil))
  (valAt [_ k not-found]
    (or (some (fn [[t v]] (when (= k v) t)) value)
        not-found))
  clojure.lang.IPersistentMap
  (without [_ key]
    (Sub. (remove (comp #{key} first) orig)
          (remove (comp #{key} first) value)))
  Show
  (show [_]
    (->> orig
         (map (fn [[term subs :as subst]]
                (if (empty? subst)
                  "[]"
                  (format "[%s / %s]" (pr-str term) (pr-str subs)))))
         (string/join " ◦ ")))
  Substitutable
  (substitute [x substitution]
    (->> value
         (mapv (fn [[term var :as subst]]
                 (if (empty? subst)
                   []
                   [(substitute term substitution) var])))
         (Sub. orig))))

(def idsub (Sub. [[]] [[]]))

(defn sub [pairs]
  (Sub. pairs pairs))

(defn singleton [var term]
  (Sub. [[term var]] [[term var]]))

(defn compose [& substitutions]
  (letfn [(-compose [s t]
            (if (instance? Sub t)
              (let [t' (substitute t s)]
                (Sub. (into (.-orig t')  (.-orig s))
                      (into (.-value t') (.-value s))))
              (throw (Exception. "Argument `t` must be a substitution"))))]
    (reduce -compose (reverse substitutions))))

(defn instantiate [state scheme]
  (let [[vars' state'] (map-m fresh state (:vars scheme))
        subst          (sub (interleave vars vars'))]
    [(substitute (:t scheme) subst) state']))

(defn generalize [env t]
  (Scheme. (set/difference (free t) (free env)) t))

(print-show Sub TVar TConst TArrow)


;;; Unification

(defn occurs?
  "A variable `x` occurs in `term` if and only if `t = f(s[1],...s[n])` for
  `n > 0` and either `s[i] = x` or `x` occurs in `s[i]` for some
  `i = 1,2,...,n`"
  [x term]
  (contains? (free term) x))

(defmulti unify* (fn [_ t u] [(type t) (type u)]))

(defn ex-infitite-type [a t]
  (throw
   (Exception. (format "Infinite Type %s %s" (pr-str a) (pr-str t)))))

(defmethod unify* [Var Var]
  [acc t1 t2]
  (if (= t1 t2)
    idsub
    (singleton t2 t1)))

(defmethod unify* [Var App]
  [acc t1 t2]
  (if (occurs? t1 t2)
    (ex-infitite-type t1 t2)
    (singleton t1 t2)))

(defmethod unify* [App Var]
  [acc t1 t2]
  (if (occurs? t2 t1)
    (ex-infitite-type t2 t1)
    (singleton t2 t1)))

(defn unify-args [s1 args1 args2]
  (if (and (empty? args1) (empty? args2))
    s1
    (let [[t1 & t1s] args1
          [t2 & t2s] args2
          s2 (trampoline unify* idsub t1 t2)]
      (recur (compose s2 s1) t1s t2s))))

(defmethod unify* [App App]
  [acc t1 t2]
  (let [[op1 & args1] (.-value t1)
        [op2 & args2] (.-value t2)
        s1            (unify* acc op1 op2)]
    (if (= (count args1) (count args2))
      (trampoline unify-args s1 args1 args2)
      (throw
       (Exception.
        (format "Arities do not match %s %s" (pr-str t1) (pr-str t2)))))))

(defn unify [t1 t2]
  (unify* idsub t1 t2))


;;; Inference

(defn infer [state env expr]
  (case (type expr)
    Var (if-let [s (get env expr)]
          (let [[t state'] (instantiate state s)]
            [[idsub t] state])
          (throw (Exception. (format "UnboundVariable %s" expr))))

    Abs (let [[x e] (:value expr)
              [tv state'] (fresh state)
              env' (extend env x (Scheme. [] tv))
              [[s1 t1] state''] (infer state' env' e)]
          [[s1 (substitute (App. [tv t1]) s1)] state''])

    App (let [[e1 e2] expr
              [tv state']        (fresh state)
              [[s1 t1] state'']  (infer env e1)
              [[s2 t2] state'''] (infer (substitute env s1) e2)
              s3                 (unify (substitute t1 s2) )])
    ))
