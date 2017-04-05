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

(defn print! [renderer t]
  (let [red (* 255 (cos01 (*  t 10)))
        col [red 0 255]
        fcol (apply u/to-color col) ]

    (do
      (p/clear-all! renderer col)
      )))


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
