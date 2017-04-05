(ns servalan.connection

  (:require 
    [servalan.messages :refer [mk-msg]]
    [taoensso.timbre :as t ]

    [servalan.protocols.connection :as connection]
    [servalan.protocols.clientactions :as client]

    [servalan.fsm :as fsm]
    [servalan.macros :refer [dochan chandler]]

    [clojure.core.async :refer [<!! >!! <! >! put! go chan go-loop] :as a]  
    [clj-uuid :as uuid]
    [clojure.pprint :as pp])
  )

(defprotocol IClientActions

  (none [this payload])

  (is-connecting [this payload]  )
  (has-connected [this payload])

  (is-disconnecting [this payload])
  (has-disconnected [this payload])

  (handling-client-msg [this payload])
  (handling-server-msg [this payload]))

(def conn-state-table
  {:client-message {:none handling-client-msg
                    has-connected handling-client-msg }

   :server-message {has-connected handling-server-msg }

   :done    {handling-client-msg has-connected
             handling-server-msg has-connected
             is-connecting, has-connected
             is-disconnecting, has-disconnected }

   :time-out {is-connecting is-disconnecting
              has-connected is-disconnecting }

   :disconnect {is-connecting is-disconnecting
                has-connected is-disconnecting } })

;; TODO fsm to keys - multi method the fucker
;; I can't debug this shit

(defrecord Connection [id req fsm ws-channel com-chan kill-chan]

  IClientActions

  (none [this payload])

  (is-connecting [this payload]
    (t/info "STATE: is-connecting") 
    (fsm/event! this :done payload))

  (has-connected [this payload]
    (t/info "STATE: has-connected") )

  (is-disconnecting [this payload]
    (t/info "STATE: is-disconnecting")  
    ;; TODO send a message to client
    (fsm/event! this :done payload))

  (has-disconnected [this payload]
    (t/info "STATE: has-disconnected")
    (connection/close! this))

  (handling-client-msg [this payload]
    (t/info "STATE: handling-client-msg")
    (fsm/event! this :done payload) )

  (handling-server-msg [this payload]
    (t/info "STATE: handling-server-msg ")
    (fsm/event! this :done payload)  ) 

  connection/IConnection

  (disconnect! [this]
    (println "graceful disconnect")
    (fsm/event! this :disconnect {}))

  (close! [this]
    (do
      (println "hard close")
      (a/put! kill-chan :disconnect)))

  (command! [this msg]
    (a/put! com-chan msg))

  (is-connected? [_]
    (let [ st (fsm/get-state fsm)]
     (not= (or
            (= st is-disconnecting )
            (= has-disconnected :none)))))

  fsm/IStateMachine

  (event! [this ev payload]
    (let [old-state (fsm/get-state fsm)
          new-state (fsm/event! fsm ev payload) ]

      (do

        (when new-state
          (let [ev-record {:old-state old-state
                           :state new-state
                           :event ev}]
            (new-state this payload)))  
        )))

  (get-state [this] (fsm/get-state fsm)))


(defn mk-connection [{:keys [ws-channel] :as req} ]

  (map->Connection {:id (-> ( uuid/v4 ) str keyword)
                    :fsm (fsm/mk-state-machine conn-state-table)
                    :req "no req"
                    :ws-channel ws-channel
                    :com-chan (chan)
                    :kill-chan (chan)}))



(defn- ping-msg [t]
  (mk-msg :ping {} t))

(defn mk-connection-process
  ;; Turn a ws request into a connection process
  ;; and return a connection record

  [req]

  (let [id (-> (uuid/v4) str keyword)

        {:keys [ws-channel com-chan kill-chan] :as conn} (mk-connection req )

        ev-fn (fn[ev msg] (fsm/event! conn ev {:msg msg :conn conn}))

        running? (atom true)

        stop-fn (fn [reason]
                  (t/error "client closing : " reason)
                  (reset! running? false)
                  (doseq [ch [ws-channel com-chan kill-chan]]
                    (a/close! ch))
                  (ev-fn {} :disconnect)) ]

    (t/info "client connected " id)

    (do
      ;; pings
      (go
        (while @running?
          (>! ws-channel (ping-msg 0))
          (ev-fn :sent-ping {}) 
          (<! (a/timeout 3000))))

      ;; kill channel
      (go
        (let [msg (<! kill-chan)]
        (ev-fn :stop (or msg {}))
        (stop-fn "killed")))

      ;; from client
      (go-loop []
               (if-let [{:keys [error] :as msg-raw} (<! ws-channel) ]
                 (if error
                   (stop-fn "error from websocket channel")

                   (do
                     (ev-fn :client-message (-> msg-raw :msg :message))
                     (recur)))
                 (do
                   (stop-fn "web socket channel returned nil"))))

      ;; from server / player
      (dochan [msg com-chan]
              (t/info "got an internal msg -> " msg)
              (ev-fn (:type msg ) (:payload msg) ) 
              )
      conn)
    
    ))

;; Some tests




