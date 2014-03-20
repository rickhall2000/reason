(ns reason.ch4
  (:use [clojure.repl]
        [clojure.core.logic]))

(defn eq-car? [l z]
  (= (first l) z))

(defn mem [x l]
  (cond
   (empty? l) false
   (eq-car? l x) l
   :else (mem x (rest l))))

(mem 'tofu '(a b tofu peas e))

(mem 'tofu '(a b peas d peas e))

(run* (out)
      (== (mem 'tofu '(a b tofu d peas e)) out))

(mem 'peas (mem 'tofu '(a b tofu d peas e)))

(mem 'tofu
     (mem 'tofu '(a b tofu d tofu e)))

(mem 'tofu
     (rest (mem 'tofu '(a b tofu d tofu e))))

(defn eq-caro [l x]
  (firsto l x))


(defn memo [x l out]
  (conde
   [(emptyo l) u#]
   [(eq-caro l x) (== l out)]
   [(fresh (d)
           (resto l d)
           (memo x d out))]))

(run 1 (out)
     (memo 'tofu '(a b tofu d tofu e) out))

(run 1 (out)
     (fresh (x)
            (memo 'tofu ['a 'b x 'd 'tofu 'e] out)))

(run* (r)
      (memo r
            '(a b tofu d tofu e)
            '(tofu d tofu e)))

(run* (q)
      (memo 'tofu '(tofu e) '(tofu e))
      (== true q))

(run* (q)
     (memo 'tofu '(tofu e) '(tofu))
     (== true q))

(run* (x)
      (memo 'tofu ['tofu 'e] ['peas x]))

(run* (out)
      (fresh (x)
             (memo 'tofu ['a 'b x 'd 'tofu 'e] out)))

(run 12 (s)
     (fresh (u)
            (memo 'tofu (lcons '(a b tofu d tofu e) s) u)))

(defn memo [x l out]
  (conde
   [(eq-caro l x) (== l out)]
   [(fresh (d)
           (resto l d)
           (memo x d out))]))

(defn rmember [x l]
  (cond
   (empty? l) ()
   (eq-car? l x) (rest l)
   :else (cons (first l)
               (rmember x (rest l)))))

(rmember 'peas '(a b peas d peas e))


(defn rembero' [x l out]
  (conde
   [(emptyo l)(== () out)]
   [(eq-caro l x) (resto l out)]
   [(fresh (res)
           (fresh (d)
                  (resto l d)
                  (rembero' x d res))
           (fresh (a)
                  (firsto l a)
                  (conso a res out)))]))

(defn rembero'' [x l out]
  (conde
   [(emptyo l)(== () out)]
   [(eq-caro l x) (resto l out)]
   [(fresh (a d res)
           (conso a d l)
           (rembero'' x d res)
           (conso a res out))]))


(run 1 (out)
     (fresh (y)
            (rembero 'peas ['a 'b y 'd 'peas 'e] out)))

(run* (out)
      (fresh (y z)
             (rembero y ['a 'b y 'd z 'e] out)))

(run* (r)
      (fresh (y z)
             (rembero' y [y 'd z 'e] [y 'd 'e])
             (== [y z] r)))

(run* (r)
      (fresh (y z)
             (rembero'' y [y 'd z 'e] [y 'd 'e])
             (== [y z] r)))
;;****************** 49 looks wrong -- is rembero wrong?
(run* (r)
      (fresh (y z)
             (rembero y (lcons y
                               (lcons 'd
                                      (lcons z 'e )))
                      (lcons y
                             (lcons 'd 'e)))
             (== [y z] r)))

**************************

(run 13 (w)
     (fresh (y z out)
            (rembero' y ['a 'b y 'd (lcons z w)] out)))

(defn surpriseo [s]
  (rembero' s ['a 'b 'c] ['a 'b 'c]))

(run* (r)
      (== 'd r)
      (surpriseo r))

(run* (r)
      (surpriseo r))

(run* (r)
      (surpriseo r)
      (== r 'b))



