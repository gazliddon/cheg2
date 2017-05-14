(ns client.client
  (:require
   [shared.connhelpers :as ch :refer [create-connection-process
                                      add-connection-fsm
                                      remove-connection-fsm ]]

   [shared.fsm :as FSM]

   [shared.msgutils :as MU]

   [shared.component.messagebus :as MB :refer [mk-message-bus] ]

   [shared.messages :refer [mk-msg]]

   [client.utils :as cu]
   [shared.utils :as su]

   [taoensso.timbre :as t
    :refer-macros [log  trace  debug  info  warn  error  fatal  report ]]


   [clojure.core.async :refer [chan <! >! put! close! timeout poll!] :as a]
   [client.html :as html]
   [shared.protocols.clientconnection :as client]
   [com.stuartsierra.component :as c]
   [chord.client :as wsockets  ])

  (:require-macros
                                        ; [cljs.spec.test :as st ]
   [client.macros :as m :refer [dochan chandler]]
   [cljs.core.async.macros :as a :refer [go go-loop]]))

(enable-console-print!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn send-ui-msg [{:keys [messages] :as this} payload]
    (MB/message messages {:type :ui :payload payload}))

(defn send-disconnected-msg [{:keys [messages] :as this}]
    (MU/send-from-nw-msg messages {:type :disconnected}))

(defmulti new-state (fn [_ ev _] (:state ev)))
(defmethod new-state :handling-local-msg
  [{:keys [ws-atom] :as this} ev payload]
  (do
    (if (put! @ws-atom payload)
      (FSM/event! this :done {} )
      (FSM/event! this :remote-socket-closed {} ))))

(defmethod new-state :handling-remote-msg
  [{:keys [messages] :as this} _ payload]
  (do
    (MU/send-from-nw-msg messages payload)
    (FSM/event! this :done {} )))

(defmethod new-state :is-disconnecting
  [{:keys [kill-chan] :as this} _ _]
  (do
    (put! kill-chan {:kill-msg "is-disconnecting"})
    (FSM/event! this :done {} ) ))

(defmethod new-state :is-connecting
  [{:keys [ws-atom com-chan kill-chan] :as this} ev url]
  (go
    (let [ch (wsockets/ws-ch url)
          {:keys [ws-channel error] :as k} (<! ch)
          event!  (fn [ev payload] (FSM/event! this ev payload)) ]

      (if error
        ;; An error
        (event! :connection-error {:error error})
        ;; hunky dory
        (do
          (reset! ws-atom ws-channel)
          (create-connection-process event! com-chan kill-chan ws-channel))))))

(defmethod new-state :has-disconnected
  [this _ _]
  (do
    (send-disconnected-msg this)
    (FSM/event! this :done {})))

(defmethod new-state :none [this ev payload])

(defmethod new-state :has-connected [this ev payload])

(defmethod new-state :default [{:keys [config] :as this} ev payload]
  (t/error "missing state entry function " (:state ev))
  (t/error "ev      -> " ev)
  (t/error "payload -> " payload))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn listen-for-msgs-to-remote!
  ;; listen on to-remote-chan for any 
  ;; messages we should send on
  [{:keys [to-remote-chan] :as this}]
  (do
    (t/info "subbing to to-network msgs")  
    (a/go-loop
      []
      (if-let [msg (<! to-remote-chan)]
        (do
          (client/send! this (:payload msg ))
          (recur))
        (t/info "to-network sub closed")))
    this))

(defrecord ClientComponent [com-chan config
                            started? fsm kill-chan messages 
                            to-remote-chan ]

  FSM/IStateMachine

  (get-state [this] (FSM/get-state @fsm))

  (event! [this ev payload] (FSM/event! @fsm ev payload))

  client/IClientConnection

  (send! [this msg]
    (do
      (FSM/event! this :local-message msg)
      this))

  (disconnect! [this]
    (do
      (FSM/event! this :disconnect {})
      this))

  (connect! [this]
    (do
      (FSM/event! this :connect (-> config :conn-config :url))
      this))

  (is-connected? [this]
    (ch/is-connected? (FSM/get-state this)))

  c/Lifecycle

  (start [this]
    (if-not started?
      (-> 
        this

        (su/add-members :started?
                        {:to-remote-chan (MB/sub-topic messages :to-remote (chan))
                         :kill-chan (chan)
                         :ws-atom (atom nil) })

        (add-connection-fsm :fsm new-state)
        (listen-for-msgs-to-remote!))

      this))

  (stop [this]
    (if started?
      (do
        (t/info "stoping connection")
        (close! to-remote-chan)
        (->
          this
          (client/disconnect!)
          (remove-connection-fsm :fsm)  
          (su/nil-members :started?)))
       
      this)))

(defn mk-client-component [] (map->ClientComponent {}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
