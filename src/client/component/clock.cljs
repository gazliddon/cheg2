(ns client.component.clock
  (:require
    [taoensso.timbre :as t ]
    [com.stuartsierra.component :refer [start-system stop-system]:as component]))

(defprotocol IClock
  (get-time [_]))

(defn get-sys-time-millis []
  (double (.now js/performance)))

(defrecord Clock [base-time]

  component/Lifecycle

  (start [this]
      (->
        this
        (component/stop)
        (assoc :base-time (get-sys-time-millis))))

  (stop [this]
    (assoc this :base-time nil))

  IClock
  (get-time [this]
    (- (get-sys-time-millis) base-time )))

(defn mk-clock []
  (map->Clock {}))

