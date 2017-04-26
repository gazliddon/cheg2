
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

(defrecord Clock [base-time]

  component/Lifecycle
  (start [this]
      (->
        this
        (component/stop)
        (assoc :base-time (System/nanoTime)))) 

  (stop [this]
    (assoc this :base-time nil))

  IClock
  (get-time [this]
    (- base-time (System/nanoTime))))

(defn mk-clock []
  (map->Clock {}))


