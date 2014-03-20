(ns reason.ch1
  (:use [clojure.core.logic]))

(run* (q) u#)

(run* (q)
      (== true q))

(run* (q)
      u#
      (== true q))

(run* (q)
      s#
      (==  true q))

(run* (r)
      s#
      (== 'corn r))

(run* (r)
      u#
      (== 'corn r))

(run* (q)
      s#
      (== false q))

(run* (x)
      (let [x false]
        (== true x)))

(run* (q)
      (fresh (x)
             (== true x)
             (== true q)))

(run* (q)
      (fresh (x)
             (== x true)
             (== true q)))

(run* (x) s#)

(run* (x)
      (let [x false]
        (fresh (x)
               (== true x))))

(run* (r)
      (fresh (x y)
             (== (lcons x (lcons y ())) r)))

(run* (s)
      (fresh (t u)
             (== (lcons t (lcons u ())) s)))

(run* (r)
      (fresh (x)
             (let [y x]
               (fresh (x)
                      (== (lcons y (lcons x (lcons y ()))) r)))))
(run* (r)
      (fresh (x)
             (let [y x]
               (fresh (x)
                      (== (lcons x (lcons y (lcons x ()))) r)))))

(run* (q)
      (== false q)
      (== true q))

(run* (q)
      (== false q)
      (== false q))

(run* (q)
      (let [x q]
        (== true x)))

(run* (r)
      (fresh (x)
             (== x r)))

(run* (q)
      (fresh (x)
             (== true x)
             (== x q)))

(run* (q)
      (fresh (x)
             (== q x)
             (== true x)))

(run* (q)
      (fresh (x)
             (== true x)
             (== x q)))

(cond
 false true
 :default false)

(cond
 false s#
 :default u#)

(conde
 [u# s#]
 [s# u#])

(conde
 ([u# u#]
    [s# s#]))

(conde
 (s# s#
     s# u#))

(run* (x)
      (conde
       [(== 'olive x) s#]
       [(== 'oil x) s#]
       [s# u#]))

(run 1 (x)
     (conde
      [(== 'olive x) s#]
      [(== 'oil x) s#]
      [s# u#]))

(run* (x)
      (conde
       [(== 'virgin x) u#]
       [(== 'olive x) s#]
       [s# s#]
       [(== 'oil x) s#]
       [s# u#]))

(run 2 (x)
     (conde
      [(== 'extra x) s#]
      [(== 'virgin x) u#]
      [(== 'olive x) s#]
      [(== 'oil x) s#]
      [s# u#]))

(run* (r)
      (fresh (x y)
              (== 'split x)
              (== 'pea y)
              (== (lcons x (lcons y ())) r)))

(run* (r)
      (fresh (x y)
             (conde
              [(== 'split x) (== 'pea y)]
              [(== 'navy x) (== 'bean y)]
              [s# u#])
             (== (lcons x (lcons y ())) r)))

(run* (r)
      (fresh (x y)
             (conde
              [(== 'split x) (== 'pea y)]
              [(== 'navy x) (== 'bean y)])
             (== (lcons x (lcons y (lcons 'soup ()))) r)))

(def teacupo
  (fn [x]
    (conde
     [(== 'tea x) s#]
     [(== 'cup x) s#])))

(run* (x)
      (teacupo x))

(run* (r)
      (fresh (x y)
             (conde
              [(teacupo x) (== true y)]
              [(== false x) (== true y)])
             (== (lcons x (lcons y ())) r)))

(run* (r)
      (fresh (x y z)
             (conde
              [(== y x) (fresh (x) (== z x))]
              [(fresh (x) (== y x)) (== z x)])
             (== (lcons y (lcons z ())) r)))

(run* (r)
      (fresh (x y z)
             (conde
              [(== y x) (fresh (x) (== z x))]
              [(fresh (x) (== y x)) (== z x)])
             (== false x)
             (== (lcons y (lcons z ())) r)))

(run* (q)
      (let [a (== true q)
            b (== false q)]
        b))

(run* (q)
      (let [a (== true q)
            b (fresh (x)
                     (== x q)
                     (== false x))
            c (conde
               [(== true q) s#]
               [s# (== false q)])]
        b))
