(ns client.client
  (:require
    [clojure.spec :as s ]
    [clojure.spec.test :as st ]
    [shared.connhelpers :refer [create-connection-process
                                add-connection-fsm
                                remove-connection-fsm ]]

    [shared.messages :refer [mk-msg]]
    [shared.fsm :refer [add-fsm
                        remove-fsm ]]

    [client.utils :as cu]

    [taoensso.timbre :as t
     :refer-macros [log  trace  debug  info  warn  error  fatal  report ]]


    [clojure.core.async :refer [chan <! >! put! close! timeout poll!] :as a]
    [client.html :as html]
    [shared.fsm :as fsm]
    [shared.protocols.clientconnection :as client]
    [com.stuartsierra.component :as c]
    [chord.client :as wsockets  ])

  (:require-macros
    [cljs.spec.test :as st ]
    [client.macros :as m :refer [dochan chandler]]
    [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti new-state (fn [_ ev _] (:state ev)))

(defmethod new-state :handling-local-msg
  [{:keys [ws-atom] :as this} ev payload]
  (do
    (t/info "handling local msg " payload)
    (put! @ws-atom payload)
    (fsm/event! this :done {} )))

(defmethod new-state :handling-remote-msg
  [{:keys [com-chan ui-chan] :as this} _ payload]
  (do
    (t/info "handling remote-msg " payload)
    (put! ui-chan (mk-msg :log payload 0))
    (put! com-chan payload)
    (fsm/event! this :done {} )))

(defmethod new-state :is-disconnecting
  [{:keys [kill-chan] :as this} _ _]
  (do
    (put! kill-chan {:kill-msg "is-disconnecting"})
    (fsm/event! this :done {} ) ))

(defmethod new-state :is-connecting
  [{:keys [ws-atom com-chan kill-chan] :as this} ev {:keys [url] :as payload}]
  (go
    (let [ch (wsockets/ws-ch url) 
          {:keys [ws-channel error] :as k} (<! ch)
          event!  (fn [ev payload] (fsm/event! this ev payload)) ]

      (if error
        ;; An error
        (do
          (println "an error!")
          (event! :connection-error {:error error})
          )
        ;; hunky dory
        (do

          (println "hunky dory!")
          (reset! ws-atom ws-channel)
          (create-connection-process event! com-chan kill-chan ws-channel))))))

(defmethod new-state :has-disconnected
  [this _ _]
  (fsm/event! this :done {}))

(defmethod new-state :none [this ev payload])
(defmethod new-state :has-connected [this ev payload])

(defmethod new-state :default [{:keys [config] :as this} ev payload]
  (t/error "missing state entry function " (:state ev))
  (t/error "ev      -> " ev)
  (t/error "payload -> " payload))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord ClientComponent [ui-chan com-chan
                            started? config fsm kill-chan ws-atom] 

  fsm/IStateMachine
  (get-state [this] (fsm/get-state @fsm))
  (event! [this ev payload] (fsm/event! @fsm ev payload))

  client/IClientConnection

  (state? [this ] (fsm/get-state @fsm))

  (disconnect! [this]
    (fsm/event! this :disconnect {}))

  (connect! [this]
    (fsm/event!
      this
      :connect {:url (:url config )
                :com-chan com-chan })  )

  c/Lifecycle

  (start [this]
    (let [this (c/stop this)]
      (->
        this
        (assoc :started? true
               :kill-chan (chan)
               :ws-atom (atom nil))

        (add-connection-fsm :fsm new-state))))

  (stop [this]
    (when started?
      (t/info "stoping connection")
      (put! kill-chan {:kill-msg "stopped"})
      (remove-connection-fsm this :fsm))

    (assoc this :started? nil
                :kill-chan nil
                :ws-atom nil)))

(defn mk-client-component [config] (map->ClientComponent {:config config }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


