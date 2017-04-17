(ns servalan.component.connections
 (:require 

    [shared.utils :as su]

    [taoensso.timbre :as t
      :refer-macros [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]
    

    [shared.messages :refer [mk-msg]]
    [clojure.core.async :refer [chan <! >! put! close! timeout poll!] :as a]
    [servalan.fsm :as fsm]
    [com.stuartsierra.component :as c]
    [chord.client :as wsockets  ]))

(def fsm-table
  {:connect  {:none :is-connecting
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


(defmulti handler (fn [_ ev _] (:state ev)))

(defmulti handler :default [this ev payload]
  (t/error "unhandled stated: " ev)
  (t/error "payload: " payload))

(defmethod handle :is-connecting
  [{:keys [com-chan kill-chan ws-channel] :as this} _ _]

  (go
    ;; Hunky dory
    (do
      (-> (:connection this)
          (reset! {:ws-channel ws-channel
                   :kill-chan kill-chan
                   :original-chan ch }))

      (fsm/event! this :done {})

      ;; Kill channel - send anything to this an all will close

      (go
        (when-let [msg (<! kill-chan)]
          (swap! connection close-all-chans!)))

      ;; handle the any local messages we receive
      (go-loop
        []
        (if-let [msg (<! com-chan)]

          (do ;; yes
              (fsm/event! this :local-message msg) 
              (recur))

          (do ;; no
              (t/info "com chan process killed")
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
              (>! kill-chan :remote-socket-closed)))))))


(defrecord Connection [fsm]

  fsm/IStateMachine

  (get-state [this ] (fsm/get-state @fsm))
  (event! [this ev payload] (fsm/event! @fsm ev payload))

  c/Lifecycle

  (start [this]
    (->
      this
      (su/add-fsm :fsm fsm-table handler)))

  (stop [this]
    (->
      this
      (su/remove-fsm this :fsm))))

