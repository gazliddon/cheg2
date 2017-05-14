(ns shared.v3
  (:require
    [shared.math :as M]))



(defrecord V3 [^double x ^double y ^double  z])

(defn mk-v3
  ([^double x ^double y ^double z]
   (->V3 x y z) )

  ([^V3 [x y z]]
   (->V3 x y z) )

  ([]
   (->V3 0 0 0)))

(defn clone [^V3 [x y z]]
  (mk-v3 x y z))

(defn v+ [^V3 [x1 y1 z1] ^V3 [x2 y2 z2]]
  (->V3 (+ x1 x2)
        (+ y1 y2)
        (+ z1 y2)))

(defn v- [^V3 [x1 y1 z1] ^V3 [x2 y2 z2]]
  (->V3 (- x1 x2)
        (- y1 y2)
        (- z1 z2)))

(defn v* [^V3 [x1 y1 z1] ^V3 [x2 y2 z2]]
  (->V3 (* x1 x2)
        (* y1 y2)
        (* z1 z2)))

(defn v-div [^V3 [x1 y1 z1] ^V3 [x2 y2 z2]]
  (->V3 (/ x1 x2)
        (/ y1 y2)
        (/ z1 z2)))

(defn sv*
  "scalar mul"
  [^V3 [x1 y1 z1] ^double v]
  (->V3 (* x1 v)
        (* y1 v)
        (* z1 v)))

(defn sv-div
  "scalar div"
  [^V3 [x1 y1 z1] ^double v]
  (->V3 (/ x1 v)
        (/ y1 v)
        (/ z1 v)))

(defn ^double dot
  "dot product"
  [^V3 [x1 y1 z1] ^V3 [x2 y2 z2]]
  (+
   (x1 * x2)
   (y1 * y2)
   (z1 * z2)))

(defn len [^V3 a]
  (M/sqrt(dot a) ) )

(defn normalize [^V3 a]
  (let [l (len a)]
    (if (not= a 0)
      (sv* a (/ 1 len))
      (mk-v3))))


