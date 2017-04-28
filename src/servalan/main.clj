(ns servalan.main
  (:require

    [servalan.component.server :refer [server-component ]]
    [servalan.component.connections :refer [connections-component]]
    [servalan.component.pinger :refer [mk-pinger] ]
    [servalan.component.clock :refer [mk-clock] ]

    [shared.component.messagebus :as MB ]

    [taoensso.timbre :as t ]
    [taoensso.timbre.appenders.core :as appenders]

    [figwheel-sidecar.repl-api :as f]

    [servalan.macros :refer [dochan chandler]]
    [clojure.core.async :refer [<!! >!! <! >! put! close! go ] :as a]
    [clj-uuid :as uuid]
    [chord.http-kit :refer [with-channel wrap-websocket-handler]]
    [org.httpkit.server :refer [run-server]]
    [com.stuartsierra.component :refer [start-system stop-system]:as c]
    [clojure.pprint :as pp])
  (:gen-class))


(defn init-logging []
  (let [cdir (System/getProperty "user.dir") ]

    (t/merge-config!
      {:appenders {:println nil
                   :spit (appenders/spit-appender {:fname (str cdir "/" "servalan.log" )})}})
    (t/info "Booting servalan")))

(defn -main [& args]
  (init-logging))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord App [server config messages]
  c/Lifecycle

  (c/start [c]
    c
    )

  (c/stop [c]
    c)) 


(def config {:port 6502})

(defn mk-system [{:keys [port ] :as config}]

  (t/info "creating system")

  (c/system-map

    :messages (MB/mk-message-bus :type)

    :clock (mk-clock)

    :config config

    :connect-ch (a/chan)

    :pinger (c/using
              (mk-pinger)
              [:clock :connections])

    :connections (c/using
                   (connections-component)
                   [:config
                    :clock
                    :messages ])

    :server (c/using
              (server-component)
              [:connect-ch :connections :config :messages ])

    :app (c/using (map->App {}) [:server :config :messages])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
