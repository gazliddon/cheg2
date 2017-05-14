(ns servalan.component.connection
  (:require

    [shared.fsm :as FSM]
    [shared.msgutils :as MU]
    [shared.utils :as su]

    [shared.connhelpers :as ch ]
    [shared.protocols.clientconnection :as client]

    [servalan.component.clock :as clock]

    [clojure.stacktrace :as st]

    [clj-uuid :as uuid]
    [clojure.core.async :as a]
    [taoensso.timbre :as t ]
    [com.stuartsierra.component :as c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-ping-time [msg]
  (-> msg :payload :ping-time ))

(defmulti on-remote-message (fn [this msg] (:type msg)))

(defmethod on-remote-message :pong [{:keys [clock] :as this} {:keys [payload] :as msg }]
  (let [t (clock/get-time clock )
        ot (get-ping-time payload) ]
    (t/info "PONG round time " (- t ot) )))

(defmethod on-remote-message :default [{:keys [messages] :as this} msg]
  ;; Punt any unhandled messages to the message bus
  (MU/send-from-nw-msg messages msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti new-state (fn [_ ev _] (:state ev)))

(defmethod new-state :handling-local-msg
  [{:keys [ws-channel] :as this} ev payload]
  (do
    (if (a/put! ws-channel payload)
      (do
        (t/info "sent on local msg to client: " payload)
        (FSM/event! this :done {} ))
      (do
        (t/error "failed to send local msg to client: " payload)
        (FSM/event! this :remote-socket-closed {} )))))

(defmethod new-state :handling-remote-msg
  [{:keys [com-chan clock] :as this} _ payload]
  (do
    (on-remote-message this payload)
    (FSM/event! this :done {})))

(defmethod new-state :is-disconnecting
  [{:keys [kill-chan] :as this} ev msg]
  (do
    (a/put! kill-chan {:kill-msg "is-disconnecting"})
    (FSM/event! this :done {} ) ))

(defmethod new-state :is-connecting
  [{:keys [ws-atom com-chan kill-chan ws-channel] :as this} ev _]
  (let [ event!  (fn [ev payload] (FSM/event! this ev payload)) ]
    (do
      (ch/create-connection-process event! com-chan kill-chan ws-channel)
      (FSM/event! this :done {}))))

(defmethod new-state :has-disconnected
  [this _ _]
  (FSM/event! this :done {}))

(defmethod new-state :none [this ev payload])
(defmethod new-state :has-connected [this ev payload])

(defmethod new-state :default [{:keys [config] :as this} ev payload]
  (t/error "missing state entry function " (:state ev))
  (t/error "ev      -> " ev)
  (t/error "payload -> " payload))

(defrecord Connection [id kill-chan com-chan ws-channel ]

  client/IClientConnection

  (send! [this msg]
    (do
      (a/put! com-chan msg)
      this))

  (connect! [this]
    (do
      (FSM/event! this :connect {})
      this))

  (disconnect! [this]
    (do
      (FSM/event! this :disconnect {})
      this))

  (is-connected? [this]
    (ch/is-connected? (FSM/get-state this)))

  FSM/IStateMachine

  (get-state [this ]
    (FSM/get-state @(:fsm this)))

  (event! [this ev payload]
    (FSM/event! @(:fsm this ) ev payload))

  c/Lifecycle

  (start [this]
    (-> (c/stop this)
        (ch/add-connection-fsm :fsm new-state)
        (client/connect!)))

  (stop [this]
    ;; Need to not allow this to be restarted ever
    (if (:fsm this)
      (->
        (client/disconnect! this)
        (ch/remove-connection-fsm :fsm))
      this)))

(defn mk-connection [req clock]
  (let [com-chan (a/chan)
        ws-channel (:ws-channel req) ]

    (->
      (map->Connection {:id (keyword (str "player-id-" (uuid/v4)) )
                        :kill-chan (a/chan)
                        :com-chan (a/chan)
                        :ws-channel ws-channel
                        :clock clock })
      (c/start))))

