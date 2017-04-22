
(ns servalan.component.clock
  (:require

    [clojure.core.async :refer [>!!] :as a]

    [shared.utils :as u :refer [every-n-millis-ch]]

    [taoensso.timbre :as t ]
    [com.stuartsierra.component :refer [start-system stop-system]:as component] )
  )


(defprotocol IClock
  (get-time [_]
            ))


(defn get-sys-time-millis []
  (double (/ (System/nanoTime) 1000000)))

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


