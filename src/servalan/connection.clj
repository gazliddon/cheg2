(ns servalan.connection

  (:require 
    [servalan.messages :refer [mk-msg]]
    [taoensso.timbre :as t ]


    [client.protocols :as cp]

    [servalan.protocols.connection :as connection]
    [servalan.protocols.clientactions :as client]

    [servalan.fsm :as fsm]
    [servalan.macros :refer [dochan chandler]]

    [clojure.core.async :refer [<!! >!! <! >! put! go chan go-loop] :as a]  
    [clj-uuid :as uuid]
    [clojure.pprint :as pp])
  )

(defn keyword-uuid []
  (keyword (str "player-id-" (uuid/v4)) ))

(def conn-state-table
  {:remote-message {:none :handling-remote-msg
                    :has-connected :handling-remote-msg }

   :local-message {:none :handling-local-msg
                    :has-connected :handling-local-msg }

   :done    {:handling-remote-msg :has-connected
             :handling-local-msg :has-connected
             :is-connecting :has-connected
             :is-disconnecting :has-disconnected }

   :disconnect {:is-connecting :is-disconnecting
                :has-connected :is-disconnecting } })

(defmulti state-change (fn [conn ev-record payload] (:state ev-record)) )

(defmethod state-change :is-connecting [this ev payload]
    (t/info "STATE: is-connecting") 
    (fsm/event! this :done payload) )


(defmethod state-change :is-disconnecting [this ev payload]
    (t/info "STATE: is-disconnecting")  
    (fsm/event! this :done payload) 
  )

(defmethod state-change :has-disconnected [this ev payload]
    (t/info "STATE: has-disconnected")  
    (fsm/event! this :done payload) 
  )

(defmethod state-change :has-connected [this ev payload]
    (t/info "STATE: has-connected")  
    (t/info  ev)
  )

(defmethod state-change :handling-remote-msg [this ev payload]
  (let [{:keys [type reply-chan ] } payload]
    (do
      (t/info "STATE: handling-remote-msg ")
      (fsm/event! this :done payload) ))
  )

(defmethod state-change :handling-local-msg [{:keys[ws-channel] :as this } ev payload]
  (do
    (t/info "handling local msg " payload)
    (put! ws-channel payload)
    (fsm/event! this :done payload)))

(defmethod state-change :default [this ev payload]
    (t/info "unhandled event " )   
    (t/info "ev -> " ev )   
    (t/info "payload - > " payload )   )

(defrecord Connection [id req fsm ws-channel com-chan kill-chan]

  cp/IStatus

  (get-status [_]
    { :state (fsm/get-state fsm) })

  connection/IConnection

  (disconnect! [this]
    (fsm/event! this :disconnect {}))

  (close! [this]
    (do
      (a/put! kill-chan :disconnect)))

  (command! [this msg]
    (fsm/event! this :local-message msg))

  (is-connected? [_]
    (let [ st (fsm/get-state fsm)]
      (or
        (= st :is-connecting )
        (= st :has-connected ))))

  fsm/IStateMachine

  (event! [this ev payload]
    (let [old-state (fsm/get-state fsm)
          new-state (fsm/event! fsm ev payload) ]

      (when new-state
        (let [ev-record {:old-state old-state
                         :state new-state
                         :ev ev}]
          (state-change this ev-record payload)))))

  (get-state [this] (fsm/get-state fsm)))


(defn mk-connection [{:keys [ws-channel] :as req} ]

  (map->Connection {:id (keyword-uuid)
                    :fsm (fsm/mk-state-machine conn-state-table)
                    :req "no req"
                    :ws-channel ws-channel
                    :com-chan (chan)
                    :kill-chan (chan)}))

;; TODO
;; This is shit
;; needs to be less shit
;; client.cljs is much better

(defn mk-connection-process
  ;; Turn a ws request into a connection process
  ;; and return a connection record

  [req]

  (let [id (keyword-uuid)

        {:keys [ws-channel com-chan kill-chan] :as conn} (mk-connection req )

        ev-fn (fn[ev msg] (fsm/event! conn ev (assoc msg :reply-chan ws-channel)))

        running? (atom true)

        stop-fn (fn [reason]
                  (do
                    (t/error "client closing : " reason)
                    (reset! running? false)
                    (doseq [ch [ws-channel com-chan kill-chan]]
                      (a/close! ch))
                    (ev-fn :disconnect {} )))]
    (do
      (go
        (<! kill-chan)
        (ev-fn :stop {})
        (stop-fn "killed"))

      ;; from remote
      (go-loop
        []
        (let [{:keys [error message] } (<! ws-channel) ]

          (if message
            (if error
              (stop-fn "error from websocket channel")

              (do
                (ev-fn :remote-message message)
                (recur)))

            ;; all gone tits
            (do
              (stop-fn "web socket channel returned nil")) )))

      (dochan [message com-chan]
              (t/info "got a local message " message)
              (ev-fn :local-message message ))
      conn)
    
    ))

;; Some tests




