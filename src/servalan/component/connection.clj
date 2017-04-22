(ns servalan.component.connection
  (:require
    [shared.fsm :as FSM]
    [clj-uuid :as uuid]

    [shared.protocols.clientconnection :as client]

    [clojure.core.async :as a :refer [<! ]]

    [shared.connhelpers :refer [add-connection-fsm
                                remove-connection-fsm
                                create-connection-process ]]

    [taoensso.timbre :as t ]

    [shared.messages :refer [mk-msg]]

    [com.stuartsierra.component :as c]))

(defmulti new-state (fn [_ ev _] (:state ev)))

(defmethod new-state :handling-local-msg
  [{:keys [ws-channel] :as this} ev payload]
  (do
    (a/put! ws-channel payload)
    (FSM/event! this :done {} )))

(defmethod new-state :handling-remote-msg
  [{:keys [com-chan ui-chan] :as this} _ payload]
  (do
    (when ui-chan
      (a/put! ui-chan (mk-msg :log payload 0)))

    (a/put! com-chan payload)

    (FSM/event! this :done {})))

(defmethod new-state :is-disconnecting
  [{:keys [kill-chan] :as this} _ _]
  (do
    (a/put! kill-chan {:kill-msg "is-disconnecting"})
    (FSM/event! this :done {} ) ))

(defmethod new-state :is-connecting
  [{:keys [ws-atom com-chan kill-chan ws-channel] :as this} ev _]
  (let [ event!  (fn [ev payload] (FSM/event! this ev payload)) ]
    (do
      (create-connection-process event! com-chan kill-chan ws-channel)
      (a/put! com-chan (mk-msg :hello-from-server {:id 1236566} 0))
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

(defrecord Connection [fsm req ws-channel kill-chan com-chan id]

  client/IClientConnection
  (connect! [this] (FSM/event! this :connect {}))
  (disconnect! [this] (FSM/event! this :disconnect {}))
  (state? [this] (FSM/get-state this))

  FSM/IStateMachine

  (get-state [this ] (FSM/get-state @fsm))

  (event! [this ev payload]
    (FSM/event! @fsm ev payload))

  c/Lifecycle

  (start [this]
    (let [this (-> (c/stop this)
                   (add-connection-fsm :fsm new-state))]
      (do
        (client/connect! this)
        this)))

  (stop [this]
    ;; Need to not allow this to be restarted ever
    (if fsm
      (->
        this
        (client/disconnect!)
        (remove-connection-fsm this :fsm))
      this)))

(defn keyword-uuid []
  (keyword (str "player-id-" (uuid/v4)) ))

(defn mk-connection [req com-chan]
  (map->Connection {:id (keyword-uuid)
                    :kill-chan (a/chan)
                    :com-chan com-chan
                    :ws-channel (:ws-channel req)
                    :req req }))

