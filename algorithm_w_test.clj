(ns algorithm-w-test
  (:require
   [algorithm-w :as w :refer :all]
   [clojure.spec.alpha :as s]))


(occurs? 'a (s/conform ::w/exp 'a))

(occurs? 'a (s/conform ::w/exp '(f a)))

(occurs? 'a (s/conform ::w/exp '((f a) b)))

(occurs? 'a (s/conform ::w/exp '((f b) a)))

(occurs? 'a (s/conform ::w/exp '((f b) b c d a)))

(occurs? 'a (s/conform ::w/exp '((f b) (g b) (h c (i a)))))

(occurs? 'a (s/conform ::w/exp '((f b) (g b) (h c (i d)))))


(substitute '{int? a} (s/conform ::w/exp 'a))

(substitute '{int? a} (s/conform ::w/exp '(f a)))

(substitute '{int? a} (s/conform ::w/exp '(f b)))

(substitute '{int? a} (s/conform ::w/exp '((f b) (g b) (h c (i a)))))

(substitute '{int? a} (s/conform ::w/exp '((f b) (g b) (h c (i d)))))

(= (unify (s/conform ::w/exp 'x) (s/conform ::w/exp 'y))
   '([[:algorithm-w/var x] y]))

(= (unify (s/conform ::w/exp 'x) (s/conform ::w/exp '(f s' s'')))
   '([[:algorithm-w/app
       {:op [:algorithm-w/var f],
        :args [[:algorithm-w/var s'] [:algorithm-w/var s'']]}]
      x]))

(= (unify (s/conform ::w/exp '(f s' s'')) (s/conform ::w/exp 'x))
   '([[:algorithm-w/app
       {:op [:algorithm-w/var f],
        :args [[:algorithm-w/var s'] [:algorithm-w/var s'']]}] x]))

(= (unify (s/conform ::w/exp 'a) (s/conform ::w/exp 'a)) [])

(unify (s/conform ::w/exp '(f s' s'' s''')) (s/conform ::w/exp '(f s' s'' x)))
