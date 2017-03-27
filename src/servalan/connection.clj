(ns servalan.connection
  (:require 
    [servalan.protocols.connection :as connection]
    [servalan.fsm :as fsm]
    [clojure.core.async :refer [<!! >!! <! >! put! go chan] :as a]  
    [clj-uuid :as uuid]
    [chord.http-kit :refer [with-channel wrap-websocket-handler]]

    [org.httpkit.server :refer [run-server]]
    [com.stuartsierra.component :refer [start stop start-system stop-system]:as component] 
    [clojure.pprint :as pp])
  )

(defprotocol IClientActions

  (none [this payload])

  (is-connecting [this payload])
  (has-connected [this payload])

  (is-disconnecting [this payload])
  (has-disconnected [this payload])

  (handling-client-msg [this payload])
  (handling-server-msg [this payload]))

(def conn-state-table
  {:client-message {none is-connecting
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


(defrecord Connection [id req fsm com-channel ws-channel stop-ch]

  IClientActions

  (none [this payload])

  (is-connecting [this payload]
    ;; check what this is if it's good then done!
    (println "is connecting")
    (fsm/event! this :done payload))

  (has-connected [this payload]
    (println "back to has connected"))

  (is-disconnecting [this payload]
    (println "is disconnecting")
    (fsm/event! this :done payload))

  (has-disconnected [this payload]
    (println "has disconnected")
    (connection/close! this))

  (handling-client-msg [this payload]
    (println "handling client message")
    (fsm/event! this :done payload) )

  (handling-server-msg [this payload]
    (println "handling server message")
    (fsm/event! this :done payload)  ) 

  connection/IConnection

  (close! [this]
    (when-not (connection/is-connected? this)
      (a/put! stop-ch :stop)))

  (command! [this msg]
    (a/put! com-channel msg))

  (is-connected? [_]
    (not= (fsm/get-state fsm) :has-disconnected))

  fsm/IStateMachine

  (event! [this ev payload]
    (let [old-state (fsm/get-state fsm)
          new-state (fsm/event! fsm ev payload) ]
      (when new-state
        (let [ev-record {:old-state old-state
           :state new-state
           :event ev}]
          (new-state this payload)))))

  (get-state [this] (fsm/get-state fsm)))


(defn mk-connection [{:keys [ws-channel] :as req} ]

  (map->Connection {:id (-> ( uuid/v4 ) str keyword)
                    :fsm (fsm/mk-state-machine conn-state-table)
                    :req req
                    :ws-channel ws-channel
                    :com-channel (chan)
                    :stop-ch (chan)}))

(defn mk-connection-process
  ;; Turn a ws request into a connection process
  ;; and return a connection record

  [req]

  (println "starting a conn process")

  (println req)

  (let [id :poo
        {:keys [ws-channel com-channel stop-ch] :as conn} (mk-connection req ) ]

  (println "pre go! ")

    (go
      (let [ keep-running (atom true) ]

        (while @keep-running

          (println "pre alts")

          (doseq [cc [stop-ch
                                  ws-channel
                                  (a/timeout 500000)
                                  com-channel]]
            (println (class cc))
            
            )
           

          (let [[msg p] (a/alts! [stop-ch
                                  ws-channel
                                  (a/timeout 500000)
                                  com-channel])

                event-fn (fn [ev] (fsm/event! conn ev {:msg msg :conn conn })) ]

            (println "about to handle message")

            ;; nil msg means a timeout
            (if (nil? msg)

              (event-fn :time-out)

              (condp = p
                stop-ch          (do
                                   (event-fn :stop)
                                   (reset! keep-running false) )
                ws-channel       (event-fn :client-message)
                com-channel      (event-fn (:event msg))

                (println "unhandled message"))
              )))))
    conn))

;; Some tests




