(ns client.client
  (:require 

    [clojure.spec :as s ]
    [clojure.spec.test :as st ]

    [shared.utils :as su]
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
(defn close-chans!
  "close any of the chans in the vec of chah-keys
  return a hash with closed channels removed"

  [chans-hash & chan-keys]
  (println (str "closing " chan-keys))

  (if chan-keys
    (->
      (fn [res k]
        (if-let [ ch (get res k)] 
          (do
            (println (str "closing " k))
            (a/close! ch)
            (dissoc res k))
          res))
        (reduce chans-hash chan-keys))
    chan-keys))

(defn close-all-chans! [chans-hash]
  (apply close-chans! chans-hash (keys chans-hash)))

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

             :is-killing-connection :has-disconnected
             :is-handling-socket-error :has-disconnected
             :is-handling-remote-socket-closed :has-disconnected

             :is-handling-connection-error :has-disconnected

             :has-disconnected :none }

   :disconnect {:is-connecting :is-disconnecting
                :has-connected :is-disconnecting }

   :kill-connection {:has-connected :is-killing-connection
                     :is-disconnecting :has-disconnected }

   :remote-socket-error {:has-connected :is-handling-socket-error}

   :remote-socket-closed {:has-connected :is-handling-remote-socket-closed}

   :connection-error {:is-connecting :is-handling-connection-error} })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti new-state (fn [_ ev _] (:state ev)))

(defn rep [ev & more]
  (t/info "Entered state " (:state ev) " from event " (:event ev))
  (when more
    (t/info more)))

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
  [{:keys [connection config] :as this} ev payload]
  (swap! connection close-all-chans!)
  (fsm/event! this :done {} ))

(defmethod new-state :is-killing-connection
  [{:keys [connection config] :as this} ev payload]
  (swap! connection close-all-chans!)
  (fsm/event! this :done {} ))

(defmethod new-state :is-handling-socket-error
  [{:keys [connection config] :as this} ev payload]
  (swap! connection close-all-chans!)
  (fsm/event! this :done this))

(defmethod new-state :is-handling-remote-socket-closed
  [{:keys [connection config] :as this} ev payload]
  (swap! connection close-all-chans!)
  (fsm/event! this :done this))

(defmethod new-state :is-connecting
  [this ev {:keys [url com-chan] :as payload}]

  (go
    (let [connection (:connection this)
          ch (wsockets/ws-ch url) 
          {:keys [ws-channel error] :as k} (<! ch)]

      (if error
        ;; An error
        (fsm/event! this :connection-error {:error error})

        ;; Hunky dory

        (let [kill-chan (chan) ]
          (do
            (-> (:connection this)
                (reset! {:ws-channel ws-channel
                         :kill-chan kill-chan }))

            (fsm/event! this :done {})

            ;; Kill channel - send anything to this an all will close
            (dochan
              [msg kill-chan]
              (swap! connection close-all-chans!)
              (fsm/event! this :kill-connection {:kill-msg msg}))

            ;; handle the any local messages we receive

            (go-loop
              []
              (if-let [msg (<! com-chan)]

                (do ;; yes
                    (fsm/event! this :local-message msg) 
                    (recur))

                (do ;; no
                    (>! kill-chan :com-chan-closed))))

            ;; handle remote message channel
            ;; we're in a go block already
            (loop []
              (if-let [{:keys [error message] :as raw-message} (<! ws-channel)]
                (if error
                  (do ;; an error!
                      (fsm/event! this :remote-socket-error {})
                      (>! kill-chan :remote-socket-error))

                  (do ;; all fine
                      (fsm/event! this :remote-message message)
                      (recur)))

                (do  ;; nil from ws-channel
                    (fsm/event! this :remote-socket-closed {})         
                    (>! kill-chan :remote-socket-closed))))))))))


(defmethod new-state :has-connected
  [{:keys [com-chan] :as this} ev payload])

(defmethod new-state :has-disconnected
  [{:keys [] :as this} ev payload]
  (fsm/event! this :done {}))

(defmethod new-state :is-handling-connection-error
  [{:keys [connection config] :as this} ev payload]
  (fsm/event! this :done {}))

(defmethod new-state :none
  [{:keys [connection config] :as this} ev payload]
  (t/info "no state : disconnected"))

(defmethod new-state :default [{:keys [connection config] :as this} ev payload]
  (t/error "missing state entry function " (:state ev))
  (t/error "ev      -> " ev)
  (t/error "payload -> " payload))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord ClientComponent [ui-chan com-chan config connection fsm ] 

  fsm/IStateMachine

  (get-state [this ] (fsm/get-state @fsm))
  (event! [this ev payload] (fsm/event! @fsm ev payload))

  client/IClientConnection

  (state? [this ] (fsm/get-state @fsm))

  (disconnect! [this] (fsm/event! this :disconnect {}))

  (connect! [this]
    (fsm/event!
      this
      :connect {:url (:url config )
                :com-chan com-chan })  )

  c/Lifecycle

  (start [this]
    (if connection
      this
      (->
        this
        (assoc :connection (atom nil))    
        (cu/add-fsm :fsm conn-state-table new-state))))

  (stop [this]
    (when connection
      (t/info "stoping connection")
      (cu/remove-fsm this :fsm)
      (client/disconnect! this))
    (assoc this :connection nil)))

(defn mk-client-component [config] (map->ClientComponent {:config config }))

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

(defn valid-client? [{:keys [connection config] :as this}]
  (and
    (atom? connection)
    (satisfies? this client/IClientConnection)
    )
  false
  )

(s/def ::valid-client valid-client? )

(s/fdef new-state
  :args (s/cat :this ::chan)
  :ret ::chan)

(st/instrument `new-state)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

