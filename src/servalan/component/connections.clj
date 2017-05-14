(ns servalan.component.connections
  (:require
    [thi.ng.geom.core.vector :as v :refer [vec3]]  

    [shared.action :refer [mk-action]]

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
(defrecord Player [connection id pos clock action])

(defn initial-player-action [pos start-time]
  (mk-action pos (vec3 0 0 0) start-time (+ start-time 5000) :none))

(defn mk-player [connection start-time pos clock]
  (map->Player {:connection connection
                :id (:id connection)
                :clock clock
                :action (initial-player-action pos start-time ) }))

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
   :ws-channel ws-channel })

(defn mk-player-msg [typ payload event-time]
  {:type typ :payload payload  :event-time event-time})

(defn msg-player! [player typ payload event-time]
  (a/put! (:to-player player) (mk-player-msg typ payload event-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- call-connection! [{:keys[connections-atom]} id method & payload]
  (let [conn (get @connections-atom id)]
    (apply method conn payload))
  nil)

(defn- call-connections!
  [{:keys [connections-atom]} method & payload]
  (doseq [[k conn] @connections-atom ]
    (apply method conn payload)))
;;;;;;;;;;

(defn part-map [f mp]
  (->
    (fn [r k v]
      (let [tst (f v)]
        (assoc-in r [tst k] v)))
    (reduce-kv {} mp)))

;;;;;;;;;;

(defn print-stats [connections]
  (let [conns @(-> connections :connections-atom)]
    (println (str "there are " (count conns) " connections"))
    (doseq [[k conn] conns]
      (println (str "connected? " (client/is-connected? conn) ))))
  :done)

(defn conn-dead? [conn]
  (not (client/is-connected? conn)))

(defn partition-connections [conns]
 (let [mp (part-map conn-dead? conns)
        to-close (or (get mp true ) {})
        to-keep (or (get mp false ) {}) ]
    (do
      [to-close to-keep])))

(defn clean-connections! [{:keys [connections-atom]}]
  (let [conns @connections-atom
        [to-close to-keep] (partition-connections conns)]
    (do
      (reset! connections-atom to-keep)
      (doseq [[_ conn] to-close] (c/stop conn))

      {:before (count conns)
       :after (count to-keep) })))


(defrecord Connections [connections-atom clock messages]
  c/Lifecycle

  (start [this]
    (do
      (t/info "starting connections component: I'm listening!")
      (->
        (c/stop this)
        (assoc :connections-atom (atom {})))))

  (stop [this]
    (when connections-atom
      (close-all! this))
    (assoc this :connections-atom nil))

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
        (clean-up! this)

        (swap! connections-atom assoc id client)

        (send!
          this id
          (mk-msg :hello-from-server {:id id } (clock/get-time clock)))
        this)))

  (send! [this id msg]
    (do
      (call-connection! this id client/send! msg)
      this))

  (broadcast! [this msg]
    (do
      (clean-up! this)
      (call-connections! this client/send! msg)
      this))

  (close-all! [this]
    (do
      (clean-up! this)
      (call-connections! this client/disconnect! )
      this)))

(defn connections-component []
  (map->Connections {} ))

