
(ns servalan.component.clock
  (:require 
    [servalan.macros :as m]

    [servalan.protocols.connections :as IConns]

    [clojure.core.async :refer [<!! >!! <! >! put! close! go go-loop chan alts!] :as a]  

    [shared.messages :refer [mk-msg]]

    [shared.utils :as u :refer [bidi-ch every-n-millis-ch]]

    [taoensso.timbre :as t ]
    [taoensso.timbre.appenders.core :as appenders]
    [com.stuartsierra.component :refer [start-system stop-system]:as component] 
    [clojure.pprint :as pp :refer [pprint]])
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


