(ns reason.ch6
  (:use [clojure.core.logic]))

(defn anyo [g]
  (conde
   [g]
   [(anyo g)]))

(def nevero (anyo u#))

(run 1 (q)
     nevero
     (== true q))

(def alwayso (anyo s#))

(run 1 (q)
     alwayso
     (== true q))

(run 5 (q)
     alwayso
     (== true q))

(defn salo [g]
     (conde
      [s#]
      [g]))

(run 1 (q)
     (salo alwayso)
     (== true q))

(run 1 (q)
     (salo nevero)
     (== true q))

(run 1 (q)
     (salo nevero)
     u#
     (== true q))

#_(run 1 (q)
     alwayso
     u#
     (== true q))

(run 1 (q)
     (conde
      [(== false q) alwayso]
      [(anyo (== true q))])
     (== true q))

;; in the book, above is conde, an below is condi
;; core.logic only has conde, which behaves like condi

(run 1 (q)
     (conde
      [(== false q) alwayso]
      [(== true q)])
     (== true q))

(run 2 (q)
     (conde
      [(== false q) alwayso]
      [(== true q)])
     (== true q))

(run 5 (q)
     (conde
      [(== false q) alwayso]
      [(anyo (== true q))])
     (== true q))

(defn teacupo [x]
  (conde
   [(== 'tea x)]
   [(== 'cup x)]))

(run 5 (r)
     (conde
      [(teacupo r)]
      [(== false r)]))

(run 5 (q)
     (conde
      [(== false q) alwayso]
      [(== true q) alwayso])
     (== true q))

(run 5 (q)
     (conde
      [alwayso]
      [nevero])
     (== true q))

(run 1 (q)
     (all
      (conde
       [(== false q)]
       [(== true q)])
      alwayso)
     (== true q))

(run 5 (q)
     (all
      (conde
       [(== false q)]
       [(== true q)])
      alwayso)
     (== true q))

(run 5 (q)
     (all
      (conde
       [s#]
       [nevero])
      alwayso)
     (== true q))
