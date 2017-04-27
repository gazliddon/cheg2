(ns servalan.component.connections
  (:require
    [taoensso.timbre :as t ]
    [servalan.component.clock :as clock]

    [servalan.component.connection :as conn]

    [shared.fsm :as fsm]

    [shared.messages :refer [mk-msg]]

    [shared.connhelpers :as ch]
    [shared.fsm :as FSM]

    [shared.protocols.clientconnection :as client]

    [servalan.component.connection :as comp.connection]

    [clojure.core.async :refer [chan <!! >!! <! >! put! go ] :as a]

    [chord.http-kit :refer [with-channel wrap-websocket-handler]]

    [org.httpkit.server :refer [run-server]]

    [com.stuartsierra.component :as c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol IConnections
  (clean-up! [ths])
  (add!  [this conn])
  (send! [this id msg])
  (broadcast! [this msg])
  (close! [this id])
  (close-all! [this]))

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

(def players (atom {}))


(defn mk-player [{:keys [ws-channel] :as req}]
  {:type :player
   :last-ping -1
   :remote (:remote-addr req)
   :id (comp.connection/keyword-uuid)
   :ws-channel ws-channel })

(defn mk-player-msg [typ payload event-time]
  {:type typ :payload payload  :event-time event-time})

(defn msg-player! [player typ payload event-time]
  (a/put! (:to-player player) (mk-player-msg typ payload event-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- call-connection! [connections id method & payload]
  (let [conn (get id connections)]
    (when conn
      (apply method conn payload)))
  nil)

(defn- call-connections!
  [connections method & payload]
  (doseq [[k conn] connections ]
    (apply method conn payload)) )

(defn- has-connection? [connections id]
  (get connections id nil))

(defn- send-to-connection! [{:keys [id com-chan]} msg]
  (t/info "sending " msg " to " id)
  (put! com-chan msg))

;;;;;;;;;;

(defn print-stats [connections]
 (let [conns @(-> connections :connections-atom)]
      (println (str "there are " (count conns) " connections"))
      (doseq [[k c] conns]
        (let [state (FSM/get-state c)]
        (println (str "not conn: " (ch/not-connected? state) " state " state))))
      ) 
 :done)

(defn clean-connections! [connections]
 (let [conns @(-> connections :connections-atom)]
   (into {} 
         (->
           (fn [[k v]]
             (let [state (FSM/get-state v)]
               (do
                 (c/stop v)
                 (ch/not-connected? state))))
           (filter conns)))))

(defrecord Connections [connections-atom clock]
  c/Lifecycle

  (start [this]
    (let [this (c/stop this)]
        (t/info "starting connections component: I'm listening!")
        (assoc this
               :connections-atom (atom {}))))

  (stop [this]
    (when connections-atom
      (t/info "stopping connections component")
      (close-all! this))
      (t/info "done stopping connections component")
      (assoc this :connections-atom nil))

  IConnections

  (clean-up! [this]
    (do
      (let [conns @connections-atom
            c-before (count conns)
            new-conns (clean-connections! conns)
            c-after (count new-conns) ]
        (println "before " c-before " after " c-after)
        (reset! connections-atom (into {} new-conns))))
    nil)

  (add! [this req]
    (do
      (let [client-connection (conn/mk-connection req)
            id (:id client-connection) ]
        (do
          (clean-up! this)
          (swap! connections-atom assoc id client-connection)
          (send-to-connection!
            client-connection
            (mk-msg :hello-from-server {:id id } (clock/get-time clock))))

        nil)))

  (send! [this id msg]
    (do
      (call-connection! @connections-atom id send-to-connection! msg)
      nil))

  (close! [this id]
    (do
      (call-connection! @connections-atom id client/disconnect!)
      (swap! @connections-atom assoc id nil)
      nil))

  (broadcast! [this msg]
    (do
      (call-connections! @connections-atom send-to-connection! msg)
      nil))

  (close-all! [this]
    (do
      (clean-up! this)
      (call-connections! @connections-atom client/disconnect! )
      nil)))

(defn connections-component []
  (map->Connections {} ))

