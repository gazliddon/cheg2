(ns servalan.component.server
  (:require
    [taoensso.timbre :as t ]

    [servalan.component.connections :as conns]
    [servalan.component.connection :as conn]

    [shared.messages :refer [mk-msg]]

    [clojure.core.async :refer [chan <!! >!! <! >! put! close! go ] :as a]

    [chord.http-kit :refer [wrap-websocket-handler]]

    [org.httpkit.server :refer [run-server]]

    [com.stuartsierra.component :as c]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IServer
  (stats [_]))


(defrecord Server [connections config server-inst]

  IServer

  (stats [this]
    (let [conns (-> connections :connections-atom)]
      (println (str "there are " (count @conns) " connections"))))

  c/Lifecycle

  (start [this]
    (if-not server-inst
      (let [handler (fn [req]
                      (->>
                        (conn/mk-connection req)
                        (c/start)
                        (conns/add! connections)))]

        (t/info "starting server component")
        (assoc this
               :state :running
               :server-inst (run-server (-> handler wrap-websocket-handler) {:port (:port config)})) )
      this))

  (stop [this]
    (if server-inst
      (do
        (t/info "stopping server component")

        (server-inst :timeout 300)

        (assoc this
               :server-inst nil
               :state nil))
      this)))

(defn server-component [] (map->Server {}) )

