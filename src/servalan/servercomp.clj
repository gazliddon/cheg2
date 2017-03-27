(ns servalan.servercomp
  (:require 

    [servalan.protocols.connections :as connections]
    [servalan.protocols.connection :as connection]
    [servalan.connection :as c]

    [servalan.fsm :as fsm]
    [clojure.core.async :refer [<!! >!! <! >! put! close! go ] :as a]  
    [clj-uuid :as uuid]
    [chord.http-kit :refer [with-channel wrap-websocket-handler]]

    [org.httpkit.server :refer [run-server]]
    [com.stuartsierra.component :refer [start stop start-system stop-system]:as component] 
    [clojure.pprint :as pp])
  )


(def all-players (atom []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def objs [{:id 0
            :type :frutz
            :start 0
            :duration 1.5
            :pos [10 10]
            :col [1 1 0] }

           {:id 1
            :type :frutz
            :start 0.5
            :duration 3
            :pos [30 10]
            :col [1 0 1] } ])

(defn in-range? [t s d]
  (and (>= t s) (< t (+ s d))))

(defn obj-in-range? [t {:keys [start duration]}]
  (in-range? t start duration))

(defn active-objs [t objs]
  (->
    (fn [os o]
      (if (obj-in-range? t o)
        (conj os o)
        os))
    (reduce '() objs)))
(def objs
  [{:type :frutz
    :start-time 1.5
    :duration 5
    :pos [10 10]
    :col [255 0 0] } 

   {:type :frutz
    :start-time 0
    :duration 5
    :pos [100 30]
    :col [255 0 255]}])


(def players (atom {}))

(defn keyword-uuid []
  (keyword (str (uuid/v4)) ))

(defn mk-player [{:keys [ws-channel] :as req}]
  {:type :player
   :last-ping -1
   :remote (:remote-addr req)
   :id (keyword-uuid)
   :ws-channel ws-channel })

(defn mk-player-msg [typ payload event-time]
  {:type typ :payload payload  :event-time event-time})

(defn msg-player! [player typ payload event-time]
  (a/put! (:to-player player) (mk-player-msg typ payload event-time)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;

(defn- call-connection! [connections id method & payload]
  (let [conn (get id connections)]
    (when conn
      (apply method conn payload)))
  nil)

(defn- call-connections!
  [connections method & payload]
  (doseq [[k conn] connections ]
    (apply method conn payload))
  nil)

(defn- has-died? [conn]
  (not (connection/is-connected? conn)))

;; Dead filter transducer
(def filter-the-dead 
  (filter (fn [id conn] (not (connection/is-connected? conn)))))

(defn- has-connection? [connections id]
  (get connections id nil))

;;;;;;;;;;

(defrecord Connections [connections-atom]

  component/Lifecycle

  (start [c]
    (stop c))

  (stop [c]
    (doto c
      (connections/close-all!)
      (connections/clean-up!))
    c ) 

  connections/IConnections

  (clean-up! [this]
    (swap! connections-atom #(into {} filter-the-dead %))
    nil)

  (add! [this conn]
    (do
      (println conn)
      (when-not (-> :id conn has-connection?)
        (swap! connections-atom assoc (:id conn) conn))
      nil))

  (send! [this id msg]
    (do
      (call-connection! @connections-atom id connection/command! msg)
      nil))

  (close! [this id]
    (do
      (call-connection! @connections-atom id connection/close!)
      (swap! @connections-atom assoc id nil)
      nil))

  (broadcast! [this msg]
    (call-connections! @connections-atom connection/command! msg))

  (close-all! [this]
    (do 
      (call-connections! @connections-atom connection/close!)
      (connections/clean-up! this)
      nil)))

(defn connections-component []
  (map->Connections
    { :connections-atom ( atom {} ) }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn connection-handler [req ]
  (c/mk-connection-process req))

(defn- mk-server [port]
  (run-server (-> #'connection-handler wrap-websocket-handler) {:port port}))

(defrecord Server [port server joined-ch state]

  component/Lifecycle

  (start [c]
    (component/stop c)

    (let [connections (:connections c)
          _ (println connections)
          handler (fn [req]
                    (pp/pprint req)
                    (let [conn (c/mk-connection-process req)]
                      (connections/add! connections conn)))]
      (assoc c
             :state :running
             :server (run-server handler {:port 6502}))))

  (stop [c]
    (do
      (when server
        (server :timeout 300)
        (assoc c
               :server nil
               :state nil)))))

(defn server-component [port] (map->Server {:port port}) )

