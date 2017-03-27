(ns servalan.servercomp
  (:require 

    [servalan.protocols.connections :as IConns]
    [servalan.protocols.connection :as IConn]
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
  (not (IConn/is-connected? conn)))

;; Dead filter transducer
(def filter-the-dead 
  (filter (fn [id conn] (not (IConn/is-connected? conn)))))

(defn- has-connection? [connections id]
  (get connections id nil))

;;;;;;;;;;

(defrecord Connections [connections-atom]

  component/Lifecycle

  (start [c]
    (stop c))

  (stop [c]
    (doto c
      (IConns/close-all!)
      (IConns/clean-up!))
    c ) 

  IConns/IConnections

  (clean-up! [this]
    (swap! connections-atom #(into {} filter-the-dead %))
    nil)

  (add! [this conn]
    (do
      (let [has-con (->>
                      (:id conn)
                      (has-connection? @connections-atom))]
      (when-not has-con
        (swap! connections-atom assoc (:id conn) conn))
      nil)))

  (send! [this id msg]
    (do
      (call-connection! @connections-atom id IConn/command! msg)
      nil))

  (close! [this id]
    (do
      (call-connection! @connections-atom id IConn/close!)
      (swap! @connections-atom assoc id nil)
      nil))

  (broadcast! [this msg]
    (call-connections! @connections-atom IConn/command! msg))

  (close-all! [this]
    (do 
      (call-connections! @connections-atom IConn/close!)
      (IConns/clean-up! this)
      nil)))

(defn connections-component []
  (map->Connections
    { :connections-atom ( atom {} ) }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- get-connections [this ]
  (:connections this))
(defrecord Server [port connections server-inst]

  component/Lifecycle

  (start [c]
    (let [c (stop c)]
     (let [handler (fn [req]
                    (let [conn (c/mk-connection-process req)]
                      (IConns/add! connections conn)))]
      (assoc c
             :state :running
             :server-inst (run-server (-> handler wrap-websocket-handler) {:port 6502}))) ))

  (stop [c]
    (if server-inst
      (do
        (server-inst :timeout 300)
        (assoc c
               :server-inst nil
               :state nil))
      c)))

(defn server-component [port] (map->Server {:port port}) )

