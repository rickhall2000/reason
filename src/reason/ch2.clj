(ns reason.ch2
  (:use [clojure.core.logic]))

(let [x (fn [a] a)
      y 'c]
  (x y))

(run* (r)
      (fresh (y x)
             (== [x y] r)))

(run* (r)
      (fresh (v w)
             (== (let [x v y w] [x y]) r)))

(first '(grape raisin pear))

(first '(a c o r n))

(run* (r)
      (firsto '(a c o r n) r))

(run* (q)
      (firsto '(a c o r n) 'a)
      (== true q))

(run* (r)
      (fresh (x y)
             (firsto [r y] x)
             (== 'pear x)))

(defn caro [p a]
  (fresh (d)
         (== (lcons a d) p)))

(run* (r)
      (firsto '(a c o r n) r))

(lcons
 (first '(grape raisin pear))
 (first '(a b c)))

(run* (r)
      (fresh (x y)
             (caro '(grape raisin pear) x)
             (caro '(a b c) y)
             (== (lcons x y) r)))

(rest '(grape raisin pear))

(first (rest '(a c o r n)))

(run* (r)
      (fresh (v)
             (resto '(a c o r n) v)
             (caro v r)))

(defn cdro [p d]
  (fresh (a)
         (== (lcons a d) p)))

(lcons
 (rest '(grape raisin pear))
 (first '((a) (b) (c))))

(run* [r]
      (fresh (x y)
             (resto '(grape raisin pear) x)
             (firsto '((a) (b) (c)) y)
             (== (lcons x y) r)))

(run* (q)
      (resto '(a c o r n) '(c o r n))
      (== true q))

(run* (x)
      (resto ['c 'o 'r 'n] [x 'r 'n]))

(run* (l)
      (fresh (x)
             (resto l ['c 'o 'r 'n])
             (firsto l x)
             (== 'a x)))

(run* (l)
      (conso '(a b c) '(d e) l))

(run* (x)
      (conso x '(a b c) '(d a b c)))

(run* (r)
      (fresh (x y z)
             (== ['e 'a 'd x] r)
             (conso y ['a z 'c] r)))

(run* (x)
      (conso x ['a x 'c] ['d 'a x 'c]))

(run* (l)
      (fresh (x)
             (== ['d 'a x 'c] l)
             (conso x ['a x 'c] l)))

(run* (l)
      (fresh (x)
             (conso x ['a x 'c] l)
             (==  ['d 'a x 'c] l)))

(defn conso' [a b c]
  (== (lcons a b) c))

(run* (l)
      (fresh (d x y w s)
             (conso w ['a 'n 's] s)
             (resto l s)
             (caro l x)
             (== 'b x)
             (resto l d)
             (firsto d y)
             (== 'e y)))

(empty? '(grape raisin pear))

(empty? ())

(run* (q)
      (emptyo '(grape raisin pear))
      (== true q))

(run* (q)
      (emptyo ())
      (== true q))

(run* (x)
      (emptyo x))

(defn nullo [a]
  (== a ()))

(= 'pear 'plum)

(= 'plum 'plum)

(defn eqo [x y]
  (== x y))

(run* (q)
      (eqo 'pear 'plum)
      (== true q))

(run* (q)
      (eqo 'plum 'plum)
      (== true q))

(defn pair? [x]
  (or (lcons? x) (and (coll? x) (seq x))))

(pair? 'pair)

(pair? '(pair))

(first '(pear))

(rest '(pear))

(lcons '(split) 'pea)

(run* (r)
      (fresh [x y]
             (== (lcons x (lcons y 'salad)) r)))


(defn pairo [p]
  (fresh (a d)
         (conso a d p)))

(run* (q)
      (pairo (lcons q q))
      (== true q))

(run* (q)
      (pairo ())
      (== true q))

(run* (q)
      (pairo 'pair)
      (== true q))

(run* (x)
      (pairo x))

(run* (r)
      (pairo (lcons r 'pear)))
