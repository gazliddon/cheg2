(ns servalan.component.pinger
  (:require 
    [servalan.macros :as m]

    [servalan.component.connections :as conns]
    [clojure.core.async :refer [>!! ]]
    [shared.messages :refer [mk-msg]]
    [shared.utils :refer [every-n-millis-ch]]

    [com.stuartsierra.component :refer [start-system stop-system]:as component] ))

(defprotocol IPinger
  (got-pong [_ id]))

(defrecord Pinger [connections timer-chan clock]

  component/Lifecycle

  (start [this]
    (let [timer-chan (every-n-millis-ch 1000)
          this (-> (component/stop this)
                   (assoc :timer-chan timer-chan)) ]
      (do

        (m/dochan
          [msg timer-chan]
          (conns/broadcast! connections (mk-msg :ping {} 0)))

        this)))

  (stop [this]
    (do
      (when timer-chan
        (>!! timer-chan :dead))  
      (assoc this :timer-chan nil)))

  IPinger
  (got-pong [this id]))

(defn mk-pinger []
  (map->Pinger {}))


