(ns reason.ch8
  (:use clojure.core.logic))

(defn >1o [n]
  (fresh (a ad dd)
         (== (lcons a (lcons ad dd)) n)))

(defn poso [n]
  (fresh (a d)
         (== (lcons a d) n)))



(defn half-addero [x y r c]
  (all
   (bit-xoro x y r)
   (bit-ando x y c)))

(run* (s)
      (fresh (x y r c)
             (half-addero x y r c)
             (== [x y r c] s)))

(defn full-addero [b x y r c]
  (fresh (w xy wz)
         (half-addero x y w xy)))))


         (half-addero w b r wz)
         (bit-xoro xy wz c)))


(declare gen-addero)

(defn addero [d n m r]
  (conde
   [(== 0 d) (== () m) (== n r)]
   [(== 0 d) (== () n) (== m r) (poso m)]
   [(== 1 d) (== () m) (addero 0 n '(1) r)]
   [(== 1 d) (== () n) (addero 0 m '(1) r)]
   [(== [1] n) (== [1] m)
    (fresh (a c)
           (== [a c] r)
           (full-addero d 1 1 a c))]
   [(== '(1) n) (gen-addero d n m r)]
   [(== '(1) m) (>1o n) (>1o r) (addero d '(1) n r)]
   [(>1o n) (gen-addero d n m r)]))

(defn gen-addero [d n m r]
  (fresh (a b c e x y z)
         (== (lcons a x) n)
         (== (lcons b y) m) (poso y)
         (== (lcons c z) r) (poso z)
         (all
          (full-addero d a b c e)
          (addero e x y z))))

(defn +o [n m k]
  (addero 0 n m k))

(declare odd-*o)

(defn *o [n m p]
  (conde
   [(== n ()) (== p ())]
   [(poso n) (== m ()) (== p ())]
   [(== n '(1)) (poso m) (== p m)]
   [(>1o n) (== (1) m) (== p n)]
   [(fresh (x z)
           (== (lcons 0 x) n) (poso x)
           (== (lcons 0 z) p) (poso z)
           (>1o m)
           (*o x m z))]
   [(fresh (x y)
           (== (lcons 1 x) n) (poso x)
           (== (lcons 0 y) m) (poso y)
           (*o m n p))]
   [(fresh (x y)
           (== (lcons 1 x) n) (poso x)
           (== (lcons 1 y) m) (poso y)
           (odd-*o x n m p))]))

;; 18
(defn odd-*o [x n m p]
  (fresh (q)
         (bound-*o q p n m)
         (*o x m q)
         (+o (lcons 0 q) m p)))
