(ns reason.ch7
  (:use clojure.core.logic
        clojure.repl))

(defn bit-xoro [x y r]
  (conde
   [(== 0 x) (== 0 y) (== r 0)]
   [(== 1 x) (== 0 y) (== r 1)]
   [(== 0 x) (== 1 y) (== r 1)]
   [(== 1 x) (== 1 y) (== r 0)]))

(run* (s)
      (fresh (x y)
             (bit-xoro x y 0)
             (== [x y] s)))

(run* (s)
      (fresh (x y)
             (bit-xoro x y 1)
             (== [x y] s)))

(run* (s)
      (fresh (x y r)
             (bit-xoro x y r)
             (== [x y r] s)))

(defn bit-ando [x y r]
  (conde
   [(== 0 x) (== 0 y) (== 0 r)]
   [(== 1 x) (== 0 y) (== 0 r)]
   [(== 0 x) (== 1 y) (== 0 r)]
   [(== 1 x) (== 1 y) (== 1 r)]))


(run* (s)
      (fresh (x y r)
             (bit-ando x y 1)
             (== [x y] s)))

(run* (s)
      (fresh (x y r)
             (bit-ando x y 0)
             (== [x y] s)))

(defn half-addero [x y r c]
  (all
   (bit-xoro x y r)
   (bit-ando x y c)))

(run* (r)
      (half-addero 1 1 r 1))

(run* (s)
      (fresh (x y r c)
             (half-addero x y r c)
             (== [x y r c] s)))

(defn full-addero [b x y r c]
  (fresh (w xy wz)
         (half-addero x y w xy)))))


         (half-addero w b r wz)
         (bit-xoro xy wz c)))

(run* (s)
      (fresh (r c)
             (full-addero 0 1 1 r c)
             (== [r c] s)))

(run* (s)
      (fresh (r c)
             (full-addero 1 1 1 r c)
             (== [r c] s)))

(run* (s)
      (fresh (b x y r c)
             (full-addero b x y r c)
             (== [b x y r c] s)))

(defn buildnum [n]
  (cond
   (zero? n) ()
   (odd? n) (cons 1 (buildnum (/ (- n 1) 2)))
   :else (cons 0 (buildnum (/ n 2)))))

(defn buildnum' [n]
  (cond
   (odd? n) (cons 1 (buildnum (/ (- n 1) 2)))
   (and (even? n) (not (zero? n)))
   (cons 0 (buildnum (/ n 2)))
   (zero? n) ()))


(defn poso [n]
  (fresh (a d)
         (== (lcons a d) n)))
(run* (q)
      (poso [0 1 1])
      (== true q))

(run* (q)
      (poso [1])
      (== true q))

(run* (q)
      (poso [])
      (== true q))

(run* (r)
      (poso r))

(defn >1o [n]
  (fresh (a ad dd)
         (== (lcons a (lcons ad dd)) n)))

(run* (q)
      (>1o '(0 1 1))
      (== true q))

(run* (q)
      (>1o '(0 1))
      (== q true))

(run* (q)
      (>1o '(1))
      (== q true))

(run* (q)
      (>1o ())
      (== q true))

(run* (r)
      (>1o r))

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

(run* (s)
      (gen-addero 1 '(0 1 1) '(1 1) s))

(run* (s)
      (fresh (x y)
            (addero 0 x y '(1 0 1))
            (== (list x y) s)))

(defn +o [n m k]
  (addero 0 n m k))

(run* (s)
      (fresh (x y)
             (+o x y '(1 0 1))
             (== (list x y) s)))

(defn -o [n m k]
  (+o k m n))

(run* (q)
      (-o '(0 0 0 1) '(1 0 1) q))

(run* (q)
      (-o '(1 0 1) '(0 0 0 1) q))
