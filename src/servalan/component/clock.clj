
(ns servalan.component.clock
  (:require 

    [clojure.core.async :refer [>!!] :as a]  

    [shared.utils :as u :refer [bidi-ch every-n-millis-ch]]

    [taoensso.timbre :as t ]
    [com.stuartsierra.component :refer [start-system stop-system]:as component] )
  )


(defprotocol IClock
  (get-time [_]
            ))

(defrecord Clock []

  component/Lifecycle

  (start [this]
    (let [this (-> this
                   (component/stop ))]
      (do
        this)))

  (stop [this]
    (comment do
      (when timer-chan
        (>!! timer-chan :dead))  
      (assoc this :timer-chan nil))
    )

  IClock
  (get-time [this]
    )
  )

(defn mk-clock []
  (map->Clock {}))


