(ns servalan.servercomp
  (:require 
    [taoensso.timbre :as t ]

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


;; TODO filter for long pings

(defn is-alive? [c]
  (IConn/is-connected? c))

(defn is-dead? [c]
  (not (is-alive? c)))
 
(defn- has-connection? [connections id]
  (get connections id nil))


;;;;;;;;;;

(defrecord Connections [connections-atom]

  component/Lifecycle

  (start [this]
    (if-not connections-atom
      (do
        (t/info "starting connections component")
        (assoc this :connections-atom (atom {})) )

      this))

  (stop [this]
    (if connections-atom
      (do
        (t/info "stopping connections component")
        (IConns/close-all! this)
        (assoc this :connections-atom nil))
      this))

  IConns/IConnections

  (clean-up! [this]
    (do


      (let [new-conns (into {} (filter (fn [[k v]]
                                         (is-alive? v)) @connections-atom))]

        (t/info "cleaning up connections " (count @connections-atom))
        (reset! connections-atom (into {} new-conns))
        (t/info "cleaned up connections " (count @connections-atom)) 
        ))

    nil)

  (add! [this conn]
    (do

      (IConns/clean-up! this)

      (let [has-con (->>
                      (:id conn)
                      (has-connection? @connections-atom))]
        (when-not has-con
          (swap! connections-atom assoc (:id conn) conn)
          (t/info "number of connections: " (count @connections-atom)))

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
  (map->Connections {} ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- get-connections [this ]
  (:connections this))

(defrecord Server [connections config server-inst ]

  component/Lifecycle

  (start [this]
    (if-not server-inst
      (let [handler (fn [req]
                      (let [conn (c/mk-connection-process req)]
                        (IConns/add! connections conn))) ]

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

