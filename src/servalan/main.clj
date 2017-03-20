(ns servalan.main
  (:require 
    [servalan.servercomp :refer [server-component]]
    [clojure.core.async :refer [<!! >!! <! >! put! close! go ] :as a]  
    [clj-uuid :as uuid]
    [chord.http-kit :refer [with-channel wrap-websocket-handler]]
    [org.httpkit.server :refer [run-server]]
    [com.stuartsierra.component :refer [start stop start-system stop-system]:as component] 
    [clojure.pprint :as pp])
  (:gen-class))

(defn to-secs [t]
  (double (/ t 1000000000)))

(defn timer [millis]
  (let [c (a/chan (a/dropping-buffer 1))]
    (a/go
      (loop [start (System/nanoTime)
             t start ]
        (<! (a/timeout millis))
        (let [nt (System/nanoTime)
              dt (- nt t) ]
          (put! c {:time (to-secs (- nt start) ) :dtime (to-secs dt )})
          (recur start (System/nanoTime)))))
    c))

(defn -main [& args]
  (println "here I am!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord App []
  component/Lifecycle

  (start [c]
    ; (pp/pp c)
    c
    )

  (stop [c]
    c
    )
  )

(defrecord Connection [in-ch]
  component/Lifecycle

  (start [c]
    c
    )

  (stop [c]
    c
    )
  )

(defn mk-system [{:keys [port ] :as config}]
  (component/system-map

    :joined-ch (a/chan)

    :server (component/using
              (server-component port)
              [:joined-ch])

    :app (component/using (map->App {}) 
                          [:server])))

(def config {:port 6502 
             })

(def sys (mk-system config))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn start! []
    (alter-var-root #'sys component/start))

(defn stop! []
    (alter-var-root #'sys component/stop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




