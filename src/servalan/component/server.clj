(ns servalan.component.server
  (:require
    [taoensso.timbre :as t ]

    [servalan.component.connections :as conns]

    [shared.messages :refer [mk-msg]]

    [chord.http-kit :refer [wrap-websocket-handler]]

    [org.httpkit.server :refer [run-server]]

    [compojure.route :refer [files not-found resources]]
    [compojure.handler :refer [site]]

    [compojure.core :refer [routes GET POST DELETE ANY context]]

    [com.stuartsierra.component :as c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol IServer
  (stats [_]))

(defn make-routes [handler]
  (routes
    (GET "/ws" [] (-> handler wrap-websocket-handler) )
    (resources "/") 
    (not-found "<p>Page not found.</p>")))

(defn create-server [routes port]
  (run-server
    (-> handler wrap-websocket-handler) {:port port}) )

(defrecord Server [connections config server-inst ]
  IServer

  (stats [this]
    )

  c/Lifecycle

  (start [this]
    (if-not server-inst
      (let [handler (fn [req]
                      (conns/add! connections req))

            my-routes (make-routes (fn [req]
                                     (conns/add! connections req)))

            server-inst (run-server
                          (-> handler wrap-websocket-handler) {:port (:port config)}) ]

        (t/info "starting server component")

        (assoc this
               :state :running
               :server-inst server-inst) )
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

