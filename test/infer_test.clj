(ns infer-test
  (:require
   [infer :refer :all]
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest is]]))

(deftest occurs?-test
  (is (false? (occurs? (parse 'a) (parse 'a))))
  (is (true?  (occurs? (parse 'a) (parse '(f a)))))
  (is (false? (occurs? (parse 'a) (parse '(f b)))))
  (is (true?  (occurs? (parse 'a) (parse '((f a) b)))))
  (is (false? (occurs? (parse 'a) (parse '((f c) b)))))
  (is (true?  (occurs? (parse 'a) (parse '((f b) a)))))
  (is (true?  (occurs? (parse 'a) (parse '((f b) b c d a)))))
  (is (true?  (occurs? (parse 'a) (parse '((f b) (g b) (h c (i a)))))))
  (is (false? (occurs? (parse 'a) (parse '((f b) (g b) (h c (i d))))))))

(deftest substitute-test
  (let [subst (parse '[a / x])]
    (is (= (parse 'a) (substitute (parse 'x) subst))))
  (let [subst (parse '[(f b x) / y])]
    (is (= (parse '(f b x)) (substitute (parse 'y) subst))))
  (let [subst (parse '([(f b x) / y] ◦ [a / x]))]
    (is (= (parse '(f a (f b a))) (substitute (parse '(f a y)) subst))))
  (let [subst (parse '([] ◦ [(f b x) / y] ◦ [a / x]))]
    (is (= (parse '(f a (f b a))) (substitute (parse '(f a y)) subst)))))

(deftest unify-test
  (is (= (substitute (parse '(f a y))
                     (unify (parse '(f a y))
                            (parse '(f x (f b x)))))
         (substitute (parse '(f x (f b x)))
                     (unify (parse '(f a y))
                            (parse '(f x (f b x)))))
         (parse '(f a (f b a))))))

(clojure.test/run-tests)
