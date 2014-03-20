(ns reason.ch5
  (:use [clojure.core.logic]))

(defn append [l s]
  (if
   (empty? l) s
   (lcons (first l)
         (append (rest l) s))))

(append '(a b c) '(d e))

(append '(a b c) ())

(append () '(d e))

(append  '(d e) 'a)

#_(defn appendo [l s out]
  (if
   (emptyo l) (== s out)
   (fresh (a d res)
          (firsto l a)
          (resto l d)
          (appendo d s res)
          (conso a res out))))

#_(defn appendo [l s out]
  (if
      (emptyo l) (== s out)
      (fresh (a d res)
             (conso a d l)
             (appendo d s res)
             (conso a res out))))

(run* (x)
      (appendo
       '(cake)
       '(tastes yummy)
       x))


(run* (x)
      (fresh (y)
             (appendo
              ['cake 'with 'ice y]
              ['tastes 'yummy]
              x)))

(run* (x)
      (fresh (y)
             (appendo
              '(cake with ice cream)
              y
              x)))

(run 1 (x)
     (fresh (y)
            (appendo ['cake 'with 'ice y] ['d 't] x)))

(run 5 (x)
     (fresh (y)
            (appendo (lcons 'cake
                            (lcons 'with
                                   (lcons 'ice y))) ['d 't] x)))

(run 5 (y)
     (fresh (x)
            (appendo (lcons 'cake
                            (lcons 'with
                                   (lcons 'ice y))) ['d 't] x)))

(run 5 (x)
     (fresh (y)
            (appendo
             (lcons 'cake
                    (lcons 'with
                           (lcons 'ice y)))
             (lcons 'd
                    (lcons 't y)) x)))

(run* (x)
      (fresh (z)
             (appendo
              (lcons 'cake
                     (lcons 'with
                            (lcons 'ice '(cream))))
              (lcons 'd
                     (lcons 't z))
              x)))

(run 6 (x)
     (fresh (y)
            (appendo x y '(cake with ice d t))))

(run 6 (y)
     (fresh (x)
            (appendo x y '(cake with ice d t))))

(run 7 (r)
     (fresh (x y)
            (appendo x y '(cake with ice d t))
            (== (lcons x y) r)))

(run 7 (x)
     (fresh (y z)
            (appendo x y z)))

(run 7 (y)
     (fresh (x z)
            (appendo x y z)))

(run 7 (z)
     (fresh (x y)
            (appendo x y z)))

(run 7 (r)
     (fresh (x y z)
            (appendo x y z)
            (== (lcons x (lcons y z)) r)))

(defn swapendo [l s out]
  (conde
   [(fresh (a d res)
           (conso a d l)
           (conso a res out)
           (swapendo d s res))]
   [(emptyo l) (== s out)]))

(run 1 (z)
     (fresh (x y)
            (swapendo x y z)))

#_(defn unwrap [x]
  (if
   ((pair? x) (unwrap (car x)))
   x))

#_(unwrap '(((pizza))))

(unwrap '((((pizza pie) with)) extra cheese))

(defn pairo [p]
  (fresh (a d)
         (conso a d p)))

(defn unwrapo [x out]
  (conde
   [(pairo x) (fresh (a)
                     (firsto x a)
                     (unwrapo a out))]
   [(== x out)]))


(run* (x)
      (unwrapo '(((pizza))) x))

(run 1 (x)
     (unwrapo x 'pizza))

(run 5 (x)
     (unwrapo x 'pizza))

(run 5 (x)
     (unwrapo x '((pizza))))

(defn flatteno [s out]
  (conde
   [(emptyo s) (== out ())]
   [(pairo s)
    (fresh (a d res-a res-d)
           (conso a d s)
           (flatteno a res-a)
           (flatteno d res-d)
           (appendo res-a res-d out))]
   [(conso s () out)]))


(run 15 (x)
     (flatteno (lcons (lcons 'a 'b) 'c) x))

(run* (x)
      (flatteno (list 'a) x))

(run* (x)
      (flatteno '((a)) x))

(run* (x)
      (flatteno '((a b) c) x))

(run* (x)
      (flatteno x '(a b c)))

(defn flattenrevo [s out]
  (conde
   [(conso s () out)]
   [(emptyo s) (== () out)]
   [(fresh (a d res-a res-d)
           (conso a d s)
           (flattenrevo a res-a)
           (flattenrevo d res-d)
           (appendo res-a res-d out))]))

(run* (x)
      (flattenrevo '((a b) c) x))

(reverse
 (run* (x)
       (flattenrevo '((a b) c) x)))

(run 2 (x)
     (flattenrevo x '(a b c)))

(run 3 (x)
     (flattenrevo x '(a b c)))

(count
 (run* (x)
       (flattenrevo '((((a (((b c))) d)))) x)))
