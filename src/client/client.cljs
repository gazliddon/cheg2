(ns client.client
  (:require 
    [cljs.spec :as s]

    [taoensso.timbre :as t
      :refer-macros [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]
    

    [servalan.messages :refer [mk-msg]]
    [cljs.core.async :refer [chan <! >! put! close! timeout poll!] :as a]
    [client.html :as html]
    [servalan.fsm :as fsm]
    [servalan.protocols.clientconnection :as client]
    [com.stuartsierra.component :as c]
    [chord.client :as wsockets  ])

  (:require-macros 
    [client.macros :as m :refer [dochan chandler]]
    [cljs.core.async.macros :refer [go go-loop]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn close-chans!
  "close any of the chans in the vec of chah-keys
  return a hash with closed channels removed"

  [chans-hash & chan-keys]

  (if chan-keys
    (->
      (fn [xs x]
        (if (get xs x)
          (do
            (a/close! x)
            (dissoc xs x))
          xs)
        reduce chans-hash chan-keys))
    chan-keys))

(defn close-all-chans! [chans-hash]
  (apply close-chans! chans-hash (keys chans-hash)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce clock (atom 0))

(defn get-time []
  (swap! clock inc)
  @clock)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def conn-state-table

  {:client-message {:has-connected :handling-client-msg }

   :server-message {:has-connected :handling-server-msg }

   :done    {:handling-client-msg :has-connected
             :handling-server-msg :has-connected
             :is-connecting :has-connected

             :is-disconnecting :has-disconnected

             :is-killing-connection :has-disconnected
             :is-handling-socket-error :has-disconnected
             :is-handling-server-socket-closed :has-disconnected

             :is-handling-connection-error :has-disconnected

             :has-disconnected :none }

   :time-out {:is-connecting :is-disconnecting
              :has-connected :is-disconnecting }

   :connect  {:none :is-connecting
              :has-disconnected :is-connecting }

   :disconnect {:is-connecting :is-disconnecting
                :has-connected :is-disconnecting }

   :kill-connection {:has-connected :is-killing-connection
                     :is-disconnecting :has-disconnected }

   :server-socket-error {:has-connected :is-handling-socket-error}

   :server-socket-closed {:has-connected :is-handling-server-socket-closed}

   :connection-error {:is-connecting :is-handling-connection-error} })


(defmulti new-state (fn [_ ev _] (:state ev)))

(defmethod new-state :handling-client-msg
  [{:keys [connection config] :as this} ev payload]
  ;; SEND THE MESSAGE
  ;; (put! com-chan payload)
  (fsm/event! this :done {} ) )

(defmethod new-state :is-disconnecting
  [this ev payload]
  (t/info "is disconneting! " (:event ev))
  (fsm/event! this :done {} ))

(defmethod new-state :is-killing-connection
  [{:keys [connection config] :as this} ev payload]
  (swap! connection close-all-chans!)
  (fsm/event! this :done {} ))

(defmethod new-state :is-handling-socket-error
  [{:keys [connection config] :as this} ev payload]
  (swap! connection close-all-chans!)
  (fsm/event! this :done this))

(defmethod new-state :is-handling-socket-error
  [{:keys [connection config] :as this} ev payload]
  (swap! connection close-all-chans!)
  (fsm/event! this :done this))

(defmethod new-state :is-handling-server-socket-closed
  [{:keys [connection config] :as this} ev payload]
  (swap! connection close-all-chans!)
  (fsm/event! this :done this))

(defmethod new-state :is-connecting
  [{:keys [connection config] :as this} ev payload]
  (go
    (let [ch (wsockets/ws-ch (:url config)) 
          {:keys [ws-channel error] :as k} (<! ch)]

      (if error
        ;; An error
        (fsm/event! this :connection-error {:error error})

        ;; Hunky dory

        (let [kill-chan (chan) ]
          (do
            (reset! @connection {:ws-channel ws-channel :kill-chan kill-chan })

            ;; handle the kill chan

            (dochan [msg kill-chan]
                  (fsm/event! this :kill-connection {}))

            (fsm/event! this :done {})

            ;; handle incoming events

            (loop []
              (if-let [{:keys [error message]} (<! ws-channel)]
                (if error
                  (fsm/event! this :server-socket-error {})

                  (do

                    (fsm/event! this :server-message {})

                    (recur)))

                (do 
                  ;; remove without closing the ws-channel
                  ;; it's already closed if we got here
                  (swap! connection dissoc :ws-channel)
                  (fsm/event! this :server-socket-closed {}))))))))))


(defmethod new-state :has-disconnected
  [{:keys [connection config] :as this} ev payload]
  (fsm/event! this :done {}))

(defmethod new-state :is-handling-connection-error
  [{:keys [connection config] :as this} ev payload]
  (fsm/event! this :done {}))

(defmethod new-state :none
  [{:keys [connection config] :as this} ev payload]
  (t/info "no state : disconnected"))

(defmethod new-state :default [{:keys [connection config] :as this} ev payload]
  (t/info "missing state entry function " (:state ev))
  (t/info "ev      -> " ev)
  (t/info "payload -> " payload))

(defrecord ClientComponent [com-chan config connection fsm ] 
  fsm/IStateMachine

  (get-state [this ]
    (fsm/get-state @fsm))

  (event! [this ev payload]
    (do
      (fsm/event! @fsm ev payload)
      nil))

  client/IClientConnection

  (state? [this ]
    (fsm/get-state @fsm))

  (disconnect! [this]
    (do
      (fsm/event! this :disconnect {})
      nil))

  (connect! [this]
    (do
      (fsm/event! this :connect {}) 
      nil))

  c/Lifecycle

  (start [this]

    (if connection
      this

      (let [conn-atom (atom nil)
            fsm-atom (atom nil)
            this (assoc this :connection conn-atom
                            :fsm  fsm-atom)]
        (do
          (reset! fsm-atom (fsm/mk-state-machine
                             conn-state-table
                             (fn [ev payload]
                               (new-state this ev payload))))
          this))))

  (stop [this]
    (when connection
      (client/disconnect! this))
    (assoc this
           :connection nil
           :fsm nil)))

(defn mk-client-component [config]
 (map->ClientComponent {:config config }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn chan? [c]
  (= (type c) cljs.core.async.impl.channels/ManyToManyChannel))

(defn atom? [c]
  (= (type c) cljs.core/Atom))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::chan chan?)
(s/def ::atom atom?)

(s/fdef make-connection-process
        :args (s/cat :url string?
                     :state ::atom
                     :kill-chan ::chan
                     :com-chan ::chan)
        :ret nil?)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

