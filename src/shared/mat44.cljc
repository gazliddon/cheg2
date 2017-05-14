(ns shared.mat44
  (:require
    [shared.hostedarray :as HA]))

(defrecord M44 [m])

(defn mk-identity []
  (->M44
    (HA/init-float-array
      1.0 0.0 0.0 0.0
      0.0 1.0 0.0 0.0
      0.0 0.0 1.0 0.0
      0.0 1.0)))

(defn mk-rot [^V3 axis ^double rot]
  )

(defn mk-scale [^V3 scale]
  )

(defn mk-translate [^V3 trans]
  )

(def m* [])

