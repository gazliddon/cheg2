(ns servalan.connection
  (:require 
    [taoensso.timbre :as t ]

    [servalan.protocols.connection :as connection]

    [servalan.fsm :as fsm]

    [clojure.core.async :refer [<!! >!! <! >! put! go chan] :as a]  
    [clj-uuid :as uuid]
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
  {:client-message {:none is-connecting
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
    (fsm/event! this :done payload))

  (has-connected [this payload]
    (t/info "TBD: back to has connected"))

  (is-disconnecting [this payload]
    (t/info "TBD: is disconnecting")
    (fsm/event! this :done payload))

  (has-disconnected [this payload]
    (t/info "TBD: has disconnected")
    (connection/close! this))

  (handling-client-msg [this payload]
    (t/info "TBD: handling client message")
    (fsm/event! this :done payload) )

  (handling-server-msg [this payload]
    (t/info "TBD: handling server message")
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
      (println (str old-state))
      (println (str new-state))
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

(defmacro dochan [[binding chan] & body]
  `(let [chan# ~chan]
     (clojure.core.async/go
       (loop []
         (if-let [~binding (clojure.core.async/<! chan#)]
           (do
             ~@body
             (recur))
           :done)))))

(defn mk-connection-process
  ;; Turn a ws request into a connection process
  ;; and return a connection record

  [req]


  (let [id (-> (uuid/v4) str keyword)

        {:keys [ws-channel com-channel stop-ch] :as conn} (mk-connection req )

        ev-fn (fn[ev msg] (fsm/event! conn ev {:msg msg :conn conn}))

        stop-fn (fn []
                  (doseq [ch [ws-channel com-channel stop-ch]]
                    (a/close! ch))) ]

    (t/info "client connected " id)
    (t/info "req" req)

    (do
      ;; pings
      (dochan [_ ws-channel]
              (<! (a/timeout 3000))
              (>! ws-channel {:type :ping})
              (ev-fn {} :sent-ping))

      ;; stop channel
      (dochan [msg stop-ch]
              (t/info "stopping connection" "a")
              (ev-fn msg :stop)
              (stop-fn))

      ;; from client
      (dochan [msg ws-channel]
              (ev-fn msg :client-message) )

      ;; from server / player
      (dochan [msg com-channel]
              (ev-fn msg (:event msg)))
      )
    
    conn))

;; Some tests




