(ns reason.ch3
  (:use [clojure.core.logic]))

(defn pair? [x]
  (or (lcons? x) (and (coll? x) (seq x))))

(defn pairo [p]
  (fresh (a d)
         (conso a d p)))


(defn listo [l]
  (conde
   [(emptyo l) s#]
   [(pairo l) (fresh (d)
                     (resto l d)
                     (listo d))]))


(run* (x)
      (listo '(a b x d)))


(run 1 (x)
     (listo (llist 'a 'b 'c x)))

(run 5 (x)
     (listo (llist 'a 'b 'c x)))

(defn lol? [l]
  (cond
   (empty? l) true
   (list? (first l)) (lol? (rest l))
   :default false))

(defn lolo [l]
  (conde
   [(emptyo l) s#]
   [(fresh (a)
           (firsto l a)
           (listo a))
    (fresh (d)
           (resto l d)
           (lolo d))]))

(run 1 (l)
     (lolo l))

(run* (q)
      (fresh (x y)
             (lolo [['a 'b] ['c x] [y 'd]])
             (== q true)))

(run 5 (x)
     (lolo (llist ['a 'b] ['c 'd] x)))

(defn twinso [s]
  (fresh (x y)
         (conso x y s)
         (conso x () y)))

(run* (q)
      (twinso ['tofu 'tofu])
      (== true q))

(run* (x)
      (twinso [x 'tofu]))

(defn twinso [s]
  (fresh (x)
         (== [x x] s)))

(defn loto [l]
  (conde
   [(emptyo l) s#]
   [(fresh (a)
           (firsto l a)
           (twinso a))
    (fresh (d)
           (resto l d)
           (loto d))]))

(run 1 (z)
     (loto (lcons ['g 'g] z)))

(run 5 (z)
     (loto (lcons ['g 'g] z)))

(run 5 (r)
     (fresh (w x y z)
            (loto [['g 'g] ['e w] [x y] z])
            (== [w [x y] z] r)))

(run 3 (out)
     (fresh (w x y z)
            (== [['g 'g] ['e w] [x y] z] out)
            (loto out)))

(defn listofo [predo l]
  (conde
   [(emptyo l) s#]
   [(fresh (a)
           (firsto l a)
           (predo a))
    (fresh (d)
           (resto l d)
           (listofo predo d))]))

(run 3 (out)
     (fresh (w x y z)
            (== [['g 'g] ['e w] [x y] z] out)
            (listofo twinso out)))

(defn loto [l]
  (listofo twinso l))

(defn eq-car? [l z]
  (= (first l) z))

(defn member? [z l]
  (cond
   (empty? l) false
   (eq-car? l z) true
   :default (member? z (rest l))))

(member? 'olive ['virgin 'olive 'oil])

(defn eq-caro [l x]
  (firsto l x))

(defn membero' [x l]
  (conde
   [(eq-caro l x) s#]
   [(fresh (a)
           (resto l a)
           (membero x a))]))

(run* (q)
      (membero 'olive ['virgin 'olive 'oil])
      (== true q))

(run 1 (y)
     (membero y ['hummus 'with 'pita]))

(run 1 (y)
     (membero y ['with 'pita]))

(run 1 (y)
     (membero y ['pita]))

(run* (y)
      (membero y []))

(run* (y)
      (membero y ['hummus 'with 'pita]))

(defn identity [l]
  (run* (y)
        (membero y l)))

(run* (x)
      (membero 'e ['pasta x 'fagioli]))

(run 1 (x)
     (membero 'e ['pasta 'e x 'fagioli]))

(run 1 (x)
     (membero 'e ['pasta x 'e 'fagioli]))

(run* (r)
      (fresh (x y)
             (membero 'e ['pasta x 'fagioli y])
             (== [x y] r)))

(run 1 (l)
     (membero 'tofu l))

(run 5 (l)
     (membero 'tofu l))

(defn pmembero [x l]
  (conde
   [(eq-caro l x) (resto l ())]
   [(fresh (d)
           (resto l d)
           (pmembero x d))]))

(run 5 (l)
     (pmembero 'tofu l))

(run* (q)
      (pmembero 'tofu ['a 'b 'tofu 'd 'tofu])
      (== true q))

(defn pembero [x l]
  (conde
   [(eq-caro l x) (resto l ())]
   [(eq-caro l x) s#]
   [(fresh (d)
           (resto l d)
           (pmembero x d))]))

(defn pmembero [x l]
  (conde
   [(eq-caro l x) (resto l ())]
   [(eq-caro l x)
    (fresh (a d)
           (resto l (lcons a d)))]
   [(fresh (d)
            (resto l d)
            (pmembero x d))]))

(run 12 (l)
     (pmembero 'tofu l))

(defn first-value [l]
  (run 1 (y)
       (membero y l)))

(first-value '(pasta e fagioli))

(defn memberrevo [x l]
  (conde
   [(fresh (d)
           (resto l d)
           (memberrevo x d))]
   [(eq-caro l x)]))

(run* (x)
      (memberrevo x '(pasta e fagioli)))
