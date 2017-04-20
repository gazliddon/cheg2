(ns servalan.component.connection
  (:require
    [servalan.fsm :as fsm]

    [shared.protocols.clientconnection :as client]

    [clojure.core.async :as a
     :refer [chan <! >! put! close! timeout poll! go]]

    [shared.connhelpers :as chelpers
     :refer [add-connection-fsm remove-connection-fsm
             create-connection-process ] ]

    [taoensso.timbre :as t
     :refer-macros [log  trace  debug  info  warn  error  fatal  report
                    logf tracef debugf infof warnf errorf fatalf reportf
                    spy get-env]]

    [shared.messages :refer [mk-msg]]
    [com.stuartsierra.component :as c]))

(defmulti new-state (fn [_ ev _] (:state ev)))

(defmethod new-state :handling-local-msg
  [{:keys [ws-channel] :as this} ev payload]
  (do
    (put! ws-channel payload)
    (fsm/event! this :done {} )))

(defmethod new-state :handling-remote-msg
  [{:keys [com-chan ui-chan] :as this} _ payload]
  (do
    (when ui-chan
      (put! ui-chan (mk-msg :log payload 0)))

    (put! com-chan payload)

    (fsm/event! this :done {})))

(defmethod new-state :is-disconnecting
  [{:keys [kill-chan] :as this} _ _]
  (do
    (put! kill-chan {:kill-msg "is-disconnecting"})
    (fsm/event! this :done {} ) ))

(defmethod new-state :is-connecting
  [{:keys [ws-atom com-chan kill-chan ws-channel] :as this} ev _]
  (let [ event!  (fn [ev payload] (fsm/event! this ev payload)) ]
    (create-connection-process event! com-chan kill-chan ws-channel)))

(defmethod new-state :has-disconnected
  [this _ _]
  (fsm/event! this :done {}))

(defmethod new-state :none [this ev payload])
(defmethod new-state :has-connected [this ev payload])

(defmethod new-state :default [{:keys [config] :as this} ev payload]
  (t/error "missing state entry function " (:state ev))
  (t/error "ev      -> " ev)
  (t/error "payload -> " payload))

(defrecord Connection [fsm req ws-channel kill-chan com-chan]

  client/IClientConnection

  (connect! [this] (fsm/event! this :connect {}))
  (disconnect! [this] (fsm/event! this :disconnect {}))
  (state? [this] (fsm/get-state this))

  fsm/IStateMachine
  (get-state [this ] (fsm/get-state @fsm))
  (event! [this ev payload] (fsm/event! @fsm ev payload))

  c/Lifecycle

  (start [this]
    (let [this (-> (c/stop this)
                   (add-connection-fsm this :fsm new-state))]
      (do
        (client/connect! this)
        this)))

  (stop [this]
    ;; Need to not allow this to be restarted ever
    (->
      this
      (client/disconnect!)
      (remove-connection-fsm this :fsm))))

(defn mk-connection [req com-chan]
  (map->Connection {:kill-chan (chan)
                    :com-chan com-chan
                    :ws-channel (:ws-channel req)
                    :req req }))

