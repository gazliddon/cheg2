(ns servalan.component.connections
  (:require
    [shared.utils :as su]
    [servalan.fsm :as fsm]

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

(defmulti handler (fn [_ ev _] (:state ev)))

(defmethod handler :default [this ev payload]
  )

(defrecord Connection [fsm]

  fsm/IStateMachine
  (get-state [this ] (fsm/get-state @fsm))
  (event! [this ev payload] (fsm/event! @fsm ev payload))

  c/Lifecycle

  (start [this]
    (->
      (c/stop this)
      (add-connection-fsm this :fsm handler)))

  (stop [this]
    (->
      this
      (remove-connection-fsm this :fsm))))


