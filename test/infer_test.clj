(ns infer-test
  (:require
   [infer]
   [syntax]
   [type]
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest is]]))

(deftest occurs?-test
  (is (true?  (infer/occurs? 'a (type/->Var 'a))))
  (is (false? (infer/occurs? (type/->Var 'a) (type/->Var 'a))))
  (is (true?  (infer/occurs? 'a
                             (type/->Arrow (type/->Var 'a)
                                           (type/->Const 'int?)))))
  (is (false?  (infer/occurs? 'b
                              (type/->Arrow (type/->Var 'a)
                                            (type/->Const 'int?)))))
  ;; (is (true?  (infer/occurs? 'a (syntax/parse '(f a)))))
  ;; (is (false? (infer/occurs? 'a (syntax/parse '(f b)))))
  ;; (is (true?  (infer/occurs? 'a (syntax/parse '((f a) b)))))
  ;; (is (false? (infer/occurs? 'a (syntax/parse '((f c) b)))))
  ;; (is (true?  (infer/occurs? 'a (syntax/parse '((f b) a)))))
  ;; (is (true?  (infer/occurs? 'a (syntax/parse '((f b) b c d a)))))
  ;; (is (true?  (infer/occurs? 'a (syntax/parse '((f b) (g b) (h c (i a)))))))
  ;; (is (false? (infer/occurs? 'a (syntax/parse '((f b) (g b) (h c (i d)))))))
  )

;; (deftest substitute-test
;;   (let [subst (syntax/parse '[a / x])]
;;     (is (= (syntax/parse 'a) (type/substitute (syntax/parse 'x) subst))))
;;   (let [subst (syntax/parse '[(f b x) / y])]
;;     (is (= (syntax/parse '(f b x)) (type/substitute (syntax/parse 'y) subst))))
;;   (let [subst (syntax/parse '([(f b x) / y] ◦ [a / x]))]
;;     (is (= (syntax/parse '(f a (f b a))) (type/substitute (syntax/parse '(f a y)) subst))))
;;   (let [subst (syntax/parse '([] ◦ [(f b x) / y] ◦ [a / x]))]
;;     (is (= (syntax/parse '(f a (f b a))) (type/substitute (syntax/parse '(f a y)) subst)))))

;; (deftest unify-test
;;   (is (= (type/substitute (syntax/parse '(f a y))
;;                           (infer/unify (syntax/parse '(f a y))
;;                                        (syntax/parse '(f x (f b x)))))
;;          (type/substitute (syntax/parse '(f x (f b x)))
;;                           (infer/unify (syntax/parse '(f a y))
;;                                        (syntax/parse '(f x (f b x)))))
;;          (syntax/parse '(f a (f b a))))))

(occurs?-test)
