(ns servalan.component.pinger
  (:require

    [clojure.spec.alpha :as s]

    [servalan.macros :as m]

    [servalan.component.clock :as clock]
    [taoensso.timbre :as t ]

    [servalan.component.connections :as conns]
    [clojure.core.async :as a ]
    [shared.messages :refer [mk-msg]]
    [shared.utils :refer [every-n-millis-ch]]

    [com.stuartsierra.component :refer [start-system stop-system]:as component] ))

(defprotocol IPinger
  (got-pong [_ id]))

(defn mk-ping-msg [t]
  (mk-msg :ping {:ping-time (int t)} t))

(defn mk-pinger-process [clock connections]
  (let [timer-chan (every-n-millis-ch 3000)]
    (do
      (a/go-loop
        []
        (if-let [msg (a/<! timer-chan)]
          (let [t (clock/get-time clock)
                msg (mk-ping-msg t)  ]
            (conns/broadcast! connections msg)
            (recur))

          (do
            (t/info "closing pinger service"))))

      timer-chan)))

(defrecord Pinger [connections timer-chan clock]
  component/Lifecycle

  (start [this]
    (if-not timer-chan
     (->
      this
      (assoc
        :timer-chan (mk-pinger-process clock connections)))
     this))

  (stop [this]
    (do
      (if timer-chan
        (do
          (a/>!! timer-chan :dead) 
          (assoc this :timer-chan nil)
          (t/info "pinger component closed"))

        this)))

  IPinger
  (got-pong [this id]))

(defn mk-pinger []
  (map->Pinger {}))


