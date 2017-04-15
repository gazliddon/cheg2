(ns servalan.component.pinger
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

(defn print-threads [& {:keys [headers pre-fn]
                     :or {pre-fn identity}}]
  (let [thread-set (keys (Thread/getAllStackTraces))
        thread-data (mapv bean thread-set)
        headers (or headers (-> thread-data first keys))]
    (clojure.pprint/print-table headers (pre-fn thread-data))))

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
          (IConns/broadcast! connections (mk-msg :ping {} 0)))

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


