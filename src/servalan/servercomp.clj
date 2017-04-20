(ns servalan.servercomp
  (:require
    [taoensso.timbre :as t ]

    [servalan.fsm :as fsm]

    [shared.messages :refer [mk-msg]]

    [shared.connhelpers :as ch]

    [servalan.protocols.connections :as IConns]

    [shared.protocols.clientconnection :as client]

    [servalan.component.connection :as comp.connection]

    [clojure.core.async :refer [<!! >!! <! >! put! close! go ] :as a]

    [chord.http-kit :refer [with-channel wrap-websocket-handler]]

    [org.httpkit.server :refer [run-server]]

    [com.stuartsierra.component :as c]

    [clojure.pprint :as pp]))

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

(defn- send-to-connection! [{:keys [com-chan]} msg]
  (put! com-chan msg))

;;;;;;;;;;

(defrecord Connections [connections-atom]
  c/Lifecycle

  (start [this]
    (let [this (c/stop this)]
        (t/info "starting connections component: I'm listening!")
        (assoc this
               :connections-atom (atom {}))))

  (stop [this]
    (when connections-atom
      (t/info "stopping connections component")
      (IConns/close-all! this))
      (assoc this :connections-atom nil)  )

  IConns/IConnections

  (clean-up! [this]
    (do
      (let [new-conns (into {} (filter (fn [[k v]]
                                         (ch/not-connected? v)) @connections-atom))]

        (t/info "cleaning up connections " (count @connections-atom))
        (reset! connections-atom (into {} new-conns))
        (t/info "cleaned up connections " (count @connections-atom))))
    nil)

  (add! [this conn]
    (do
      (IConns/clean-up! this)

      (let [id (:id conn)
            has-con (has-connection? @connections-atom conn)]

        (when-not has-con
          (swap! connections-atom assoc id conn)
          (send-to-connection! conn (mk-msg :hello-from-server {:id id } 0)))

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
      (call-connections! @connections-atom client/disconnect! )
      (IConns/clean-up! this)
      nil)))

(defn connections-component []
  (map->Connections {} ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Server [connections config server-inst]

  c/Lifecycle

  (start [this]
    (if-not server-inst
      (let [handler (fn [req]
                      (->>
                        (comp.connection/mk-connection req)
                        (c/start)
                        (IConns/add! connections)))]

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

