(ns shared.action
  (:require
    [thi.ng.geom.core :as g]
    [thi.ng.geom.core.vector :as v :refer [vec3]]))

(defprotocol IAction
  (is-alive? [_ t])
  (get-movement-at-t [_ t]))

(defmulti get-pos-interp (fn [this t] (:kind this)))

(defmethod get-pos-interp :default
  [{:keys [pos vel] :as this} t]
  {:vel vel
   :pos pos })

(defmethod get-pos-interp :linear
  [{:keys [pos vel] :as this} t]
  {:vel vel
   :pos (g/* pos (vec3 t t t))})

(defmethod get-pos-interp :acc
 [{:keys [pos vel params] :as this} t]
 (let [{:keys [acc]} params
       halftsq (* 0.5 (* t t)) ]
   {:pos (g/+ (g/* vel t)
              pos
              (g/* acc halftsq))
    :vel (g/+ vel (g/* acc t))}))

(defrecord Action [pos vel start-time duration kind params]
  IAction

  (is-alive? [this t]
    (and
      (>= t start-time)
      (<  t (+ start-time duration))))

  (get-movement-at-t [this t]
    (when (is-alive? this t)
      (get-pos-interp this (- t start-time)))))

(defn mk-action
  ([pos vel start-time duration kind params]
   (map->Action {:pos pos
                 :vel vel
                 :start-time start-time
                 :duration duration
                 :kind kind
                 :params params } ))

  ([pos vel start-time duration kind]
   (mk-action pos vel start-time duration kind {})))
