(ns servalan.component.pinger
  (:require
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
            (t/info "closing pinger"))))

      timer-chan  )))

(defrecord Pinger [connections timer-chan clock]
  component/Lifecycle

  (start [this]
    (->
      this
      (component/stop)
      (assoc
        :timer-chan (mk-pinger-process clock connections))))

  (stop [this]
    (do
      (when timer-chan
        (a/>!! timer-chan :dead))
      (assoc this :timer-chan nil)))

  IPinger
  (got-pong [this id]))

(defn mk-pinger []
  (map->Pinger {}))


