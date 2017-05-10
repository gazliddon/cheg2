(ns client.component.clock
  (:require

    [shared.utils :as su]

    [taoensso.timbre :as t ]
    [com.stuartsierra.component :refer [start-system stop-system]:as component]))

(defprotocol IClock
  (get-time [_]))

(defn get-sys-time-millis []
  (double (.now js/performance)))

(defrecord Clock [base-time started?]

  component/Lifecycle

  (start [this]
    (if-not started?
      (->
        this
        (assoc :started? true
               :base-time (get-sys-time-millis)))
      this))

  (stop [this]
    (if started?
      (->
        this
        (assoc 
          :started? nil
          :base-time nil)  )

      this))

  IClock
  (get-time [this]
    (- (get-sys-time-millis) base-time )))

(defn mk-clock []
  (map->Clock {}))

