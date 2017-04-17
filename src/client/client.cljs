(ns client.client
  (:require 


    [clojure.spec :as s ]
    [clojure.spec.test :as st ]

    [shared.connhelpers :refer [create-connection-process ] :as ch]

    [shared.utils :refer [add-fsm
                          remove-fsm ] :as su]

    [client.utils :as cu]

    [taoensso.timbre :as t
      :refer-macros [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]
    

    [shared.messages :refer [mk-msg]]
    [clojure.core.async :refer [chan <! >! put! close! timeout poll!] :as a]
    [client.html :as html]
    [servalan.fsm :as fsm]
    [shared.protocols.clientconnection :as client]
    [com.stuartsierra.component :as c]
    [chord.client :as wsockets  ])

  (:require-macros 
    [cljs.spec.test :as st ]
    [client.macros :as m :refer [dochan chandler]]
    [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce clock (atom 0))

(defn get-time []

  (swap! clock inc)

  @clock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; local = from me, the client
;; remote = from the remote host

(def conn-state-table

  {:listen {:none :is-listening }

   :connect  {:none :is-connecting
              :has-disconnected :is-connecting }

   :local-message {:has-connected :handling-local-msg }

   :remote-message {:has-connected :handling-remote-msg }

   :done    {:handling-local-msg :has-connected
             :handling-remote-msg :has-connected
             :is-connecting :has-connected
             :is-disconnecting :has-disconnected
             :has-disconnected :none }

   :disconnect {:is-connecting :is-disconnecting
                :has-connected :is-disconnecting }

   :remote-socket-error {:is-connecting :is-disconnecting
                         :has-connected :is-disconnecting }

   :remote-socket-closed {:is-connecting :is-disconnecting
                          :has-connected :is-disconnecting }

   :connection-error {:is-connecting :is-disconnecting
                      :has-connected :is-disconnecting } })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti new-state (fn [_ ev _] (:state ev)))

(defmethod new-state :handling-local-msg
  [{:keys [ws-channel] :as this} ev payload]
  (do
    (put! ws-channel payload)
    (fsm/event! this :done {} )))

(defmethod new-state :handling-remote-msg
  [{:keys [com-chan] :as this} ev payload]
  (do
    (put! com-chan payload) 
    (fsm/event! this :done {} )))

(defmethod new-state :is-disconnecting
  [{:keys [config kill-chan] :as this} ev payload]
  (put! kill-chan {:kill-msg "is-disconnecting"})
  (fsm/event! this :done {} ))

(defmethod new-state :is-connecting
  [{:keys [com-chan kill-chan] :as this} ev {:keys [url] :as payload}]

  (go
    (let [ch (wsockets/ws-ch url) 
          {:keys [ws-channel error] :as k} (<! ch)
          event!  (fn [ev payload] (fsm/event! this ev payload)) ]

      (if error
        ;; An error
        (event! :connection-error {:error error})
        ;; hunky dory
        (create-connection-process event! com-chan kill-chan ws-channel)))))

(defmethod new-state :has-connected
  [{:keys [com-chan] :as this} ev payload])

(defmethod new-state :has-disconnected
  [{:keys [] :as this} ev payload]
  (fsm/event! this :done {}))

(defmethod new-state :none
  [{:keys [config] :as this} ev payload]
  (t/info "no state : disconnected"))

(defmethod new-state :default [{:keys [config] :as this} ev payload]
  (t/error "missing state entry function " (:state ev))
  (t/error "ev      -> " ev)
  (t/error "payload -> " payload))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord ClientComponent [started? ui-chan com-chan config fsm kill-chan] 

  fsm/IStateMachine

  (get-state [this ] (fsm/get-state @fsm))
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
               :kill-chan (chan) )

        (add-fsm :fsm conn-state-table new-state))))

  (stop [this]
    (when started?
      (t/info "stoping connection")
      (put! kill-chan {:kill-msg "stopped"})
      (remove-fsm this :fsm))
       
    (assoc this :started? nil
                :kill-chan nil)))

(defn mk-client-component [config] (map->ClientComponent {:config config }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn chan? [c]
  (= (type c) cljs.core.async.impl.channels/ManyToManyChannel))

(defn atom? [c]
  (= (type c) cljs.core/Atom))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

