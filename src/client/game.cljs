(ns client.game

  (:require

    [com.stuartsierra.component :as c]

    [servalan.messages :refer [mk-msg]]

    [client.protocols :as p] 

    [client.utils :refer [ch->coll cos cos01] :as u]

    [cljs.core.async :refer [chan <! >! put! close! timeout poll!] :as a]
    )

  (:require-macros 
    [servalan.macros :as m :refer [dochan]]
    [cljs.core.async.macros :refer [go go-loop]]))


(def player (atom {:pos [10 10]}))

(def k->dir {
   :z [-1 0]
   :x [ 1 0]
   :k [0 -1]
   :m [0 1] })

(defn addv2 [[x y] [x1 y1]] [(+ x x1) (+ y y1)])

(defn keys->player [player key-coll]
  (->
    (fn [res {:keys [event keypress]}]
      (if (= event :keydown)
        (let [v (get k->dir keypress [0 0])
              p (addv2 (:pos res) v) ]
          (assoc res :pos p))
        res))
    (reduce player key-coll)))


(comment defn update! [io timer ]
  (let [renderer (renderer io)
        now (/ ( now timer ) 1000    )
        col [(int (* 255 (cos01 (* 10 now )) )) 0 0]
        new-t (mod now 5)
        new-player (keys->player @player (keyboard io)) ]

    (do
      (clear-buffer! renderer  col)
      (draw-objs @objs @t renderer  )
      (square! renderer (:pos new-player) [10 10] [255 255 255] )
      (reset! player new-player))))

(defn is-active? [{:keys [start-time duration]} t]
  (and (>= t start-time) (< t (+ start-time duration))))

(defn perc-through [{:keys [start-time duration]} t]
  (/ (- t start-time) duration ))

(defmulti draw-obj (fn [obj t ctx]
                     (:type obj)) )

(defmethod draw-obj :frutz [{:keys [pos col]} t ctx]
  (let [[x y] pos
        new-pos [(+ x (* 100 (cos01 (* 3 t )))) y] ]
    (p/square! ctx new-pos [30 30] col) ))

(defmethod draw-obj :player [{:keys [pos col]} t ctx]
  (p/square! ctx pos [30 30] col))

(defn draw-objs
  "convert these objs into a draw list"
  [ctx objs t]
  (dorun
    (->
      (fn [o]
        (when (is-active? o t)
          (let [my-t (- (:start-time o) t)]
            (draw-obj o my-t ctx) )))
      (map objs))))

(def objs (atom []))

(defn print! [renderer t]
  (let [red (* 255 (cos01 (*  t 10)))
        col [red 0 255]
        fcol (apply u/to-color col) ]

    (do
      (doto renderer

        (p/clear-all! col)
        (draw-objs @objs t)
        (p/square! (:pos @player) [10 10] [255 255 255] )
        
        ))))

(defprotocol IGame
  (on-network [_ msg])
  (on-update [_ t])
  (on-message [_ msg] ))

(defrecord Game [started events system]

  IGame

  (on-network [this msg]
    )

  (on-update [_ t]
    (print! system t))

  (on-message [_ msg]
    (println msg))

  c/Lifecycle

  (start [this]
    (let [this (c/stop this)
          anim-ch (p/anim-ch events) ]
 
      (go-loop
        []
        (if-let [msg (<! ( p/events-ch events ))]
          (do
            (on-message this msg)
            (recur)  )
          (println "quitted events listening")))

      (go-loop
        [t 0]
        (if-let [msg (<! anim-ch)] 
          (do
            (on-update this t)
            (recur (+ t (/ 1 60))))

          (println "quitted anim listening")))

      (assoc this :started true)))

  (stop [this]
    (when started
      ;; shutdown
      )
    (assoc this :started nil)))


(defn mk-game[]
  (map->Game {}))
