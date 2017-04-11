(ns servalan.main
  (:require 
    [taoensso.timbre :as t ]
    [taoensso.timbre.appenders.core :as appenders]

    [figwheel-sidecar.repl-api :as f]  
    [servalan.servercomp :refer [server-component connections-component]]
    [servalan.macros :refer [dochan chandler]]
    [clojure.core.async :refer [<!! >!! <! >! put! close! go ] :as a]  
    [clj-uuid :as uuid]
    [chord.http-kit :refer [with-channel wrap-websocket-handler]]
    [org.httpkit.server :refer [run-server]]
    [com.stuartsierra.component :refer [start-system stop-system]:as component] 
    [clojure.pprint :as pp])
  (:gen-class))


(defn to-secs [t]
  (double (/ t 1000000000)))

(defn mk-timer-chan

  ([millis dest-ch]

   (a/go
     (loop [start (System/nanoTime) t start ]

       (let [_  (<! (a/timeout millis))
             nt (System/nanoTime)
             dt (- nt t) ]

         (when (put! dest-ch {:start (to-secs start)
                              :time (to-secs (- nt start) )
                              :dtime (to-secs dt )})

           (recur start (System/nanoTime))))))
   dest-ch)

  ([millis]
   (mk-timer-chan millis (a/chan (a/dropping-buffer 1)))))


(defn init-logging []
  (t/merge-config!
    {:appenders {:println nil
                 :spit (appenders/spit-appender {:fname "/Users/garyliddon/development/cheg2/servalan.log"})}})
  (t/info "Booting servalan") )

(defn -main [& args]
  (init-logging))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord App []
  component/Lifecycle

  (start [c]
    c) 

  (stop [c]
    c))

(def config {:port 6502})

(defn mk-system [{:keys [port ] :as config}]
  (t/info "creating system")

  (component/system-map

    :config config

    :connect-ch (a/chan)

    :connections (component/using 
                   (connections-component) 
                   [:config])

    :server (component/using
              (server-component)
              [:connect-ch :connections :config])

    :app (component/using (map->App {}) 
                          [:server :config])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



