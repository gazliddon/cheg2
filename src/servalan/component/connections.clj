(ns servalan.component.connections
  (:require
    [thi.ng.geom.core.vector :as v :refer [vec3]]  

    [shared.action :refer [mk-action]]
    [shared.component.messagebus :as MB]
    [shared.component.state :as ST]

    [taoensso.timbre :as t ]
    [servalan.component.clock :as clock]

    [servalan.component.connection :as conn]

    [shared.messages :refer [mk-msg]]
    [shared.fsm :as FSM]

    [shared.connhelpers :as ch]

    [shared.protocols.clientconnection :as client]

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
  (close-all! [this]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord PlayerConn [state korks started? client]

  ST/IStateClient
  (get-korks [_] korks)
  (get-state-holder [_] state)

  client/IClientConnection
  (send! [this msg] (client/send! this msg))
  (connect! [this] (client/connect! this))
  (disconnect! [this] (client/disconnect! this))
  (is-connected? [this] (client/is-connected? this)) 

  c/Lifecycle

  (start [this]
    (if-not started?
      (do

        this)

      this))

  (stop [this]
    (if started?
      (do
        (client/disconnect! client)
        (c/stop client)
        (ST/set-state! this nil)
        (assoc this :started? nil))
      this )))

(defn mk-player-conn [this id client]
  (let [cs nil]
    (->
      (map->PlayerConn (merge
                         (ST/child-state this id)             
                         {:client client }))
      (c/start))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-conns! [this]
  (ST/get-state this))

(defn part-map [f mp]
  (->
    (fn [r k v]
      (let [tst (f v)]
        (assoc-in r [tst k] v)))
    (reduce-kv {} mp)))

(defn do-connections [{:keys [] :as this} func]
  (doseq [[k conn] (ST/get-state this)]
          (func conn)))

(defn print-stats [this]
  (->
    (fn [{:keys [client player]}]
      (println (str "connected? " (client/is-connected? client) )))
    (do-connections))
  :done)

(defn conn-dead? [conn]
  (not (client/is-connected? conn)))

(defn partition-connections [conns]
 (let [mp (part-map conn-dead? conns)
        to-close (or (get mp true ) {})
        to-keep (or (get mp false ) {}) ]
    (do
      [to-close to-keep])))

(defn clean-connections! [{:keys [messages] :as this}]
  (let [conns (ST/get-state this)
        [to-close to-keep] (partition-connections conns)]
    (do
      (ST/set-state! this to-keep)
      (doseq [[id conn] to-close]
        (MB/message messages {:type :remove-player :id id})
        (c/stop conn))

      {:before (count conns)
       :after (count to-keep) })))

(defrecord Connections [started? state clock messages]

  ST/IStateClient

  (get-korks [_] [:players])
  (get-state-holder [_] state)

  c/Lifecycle

  (start [this]
    (if-not started?
      (do
        (t/info "starting connections component: I'm listening!")
        (ST/set-state! this {})
        (assoc this :started? true))
      this))

  (stop [this]
    (if started?
      (do
        (close-all! this) 
        (assoc this :started? nil) )
      this))

  IConnections

  (clean-up! [this]
    (let [stats (clean-connections! this)
          culled (- (:after stats) (:before stats)) ]
      (when (not= culled 0)
        (t/info "Culled " culled " connections")))
    this)

  (add! [this req]
    (let [client (conn/mk-connection req clock)
          id (:id client) ]
      (do
        (mk-player-conn this id client)  
        (clean-up! this)))) 

  (send! [this id msg]
    (do
      (client/send! (ST/get-state this id) msg)
      this))

  (broadcast! [this msg]
    (do
      (doseq [ [id conn] (ST/get-state this) ]
        (client/send! conn msg))

      (clean-up! this)

      this))

  (close-all! [this]
    (do
      (doseq [ [id conn] (ST/get-state this) ]
        (c/stop conn))
      (clean-up! this)
      this))

)

(defn connections-component []
  (map->Connections {} ))

