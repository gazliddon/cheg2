(ns user
  (:require
    [taoensso.timbre :as t ]
    [servalan.main :as main]
    [servalan.component.connections :as conns]
    [servalan.component.server :as SERVER]
    [com.stuartsierra.component :as component] 
    [clojure.tools.namespace.repl :refer (refresh)]
    [figwheel-sidecar.repl-api :as f])
  (:gen-class))

(defn fig-start
  "This starts the figwheel server and watch based auto-compiler."
  []
  (f/start-figwheel!))

(defn fig-stop
  "Stop the figwheel server and watch based auto-compiler."
  []
  (f/stop-figwheel!))

(defn cljs-repl
  "Launch a ClojureScript REPL that is connected to your build and host environment."
  []
  (f/cljs-repl))

(def system nil)

(def config {:port 6502})

(defn init []
  (alter-var-root #'system
    (constantly (main/mk-system config))))

(defn start []
  (alter-var-root #'system component/start))

(defn stop []
  (alter-var-root #'system
    (fn [s] (when s (component/stop s)))))

(defn kill-all []
  (->
    (:connections system)
    (conns/close-all!)))

(defn go []
  (main/init-logging)
  (init)
  (start))

(defn reset []
  (stop)
  (refresh :after 'user/go))

(defn stats []
  (let [server (-> system :server)]
    (SERVER/stats server)))

