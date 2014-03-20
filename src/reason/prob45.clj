(ns reason.prob45
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

(run 5 (r)
     (fresh (w x y z)
            (loto (lcons [['g 'g] ['e w] [x y]] z))
            (== [w [x y] z] r)))

(run 2 (x)
     (fresh (y)
            (loto
             (lcons ['g 'g] y))
            (firsto y 'e)
            (== x y)))

(run* (x)
      (fresh (y)
             (firsto y 'e)
             (twinso y)
             (== x y)))
