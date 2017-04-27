(ns shared.connhelpers
  (:require
    [shared.fsm :as FSM2]

    [taoensso.timbre :as t
     :refer-macros [log  debug  info  warn  error  fatal  report ]]

    #?(:clj
       [clojure.core.async :as a :refer [chan <! >! put!
                                   close! go-loop alts!
                                   sliding-buffer timeout go]]
       :cljs
       [cljs.core.async :as a :refer [chan
                                      put!
                                      close! <! >!
                                      alts! sliding-buffer timeout]])

    #?(:clj
       [clojure.core.async.impl.protocols :as p]
       :cljs
       [cljs.core.async.impl.protocols :as p]))

  #?(:cljs
     (:require-macros [cljs.core.async.macros :as a :refer [go go-loop]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn close-chans!
  "close any of the chans1in the vec of chah-keys
  return a hash with closed channels removed"

  [chans-hash & chan-keys]

  (if chan-keys
    (->
      (fn [res k]
        (if-let [ ch (get res k)]
          (do
            (a/close! ch)
            (dissoc res k))
          res))
        (reduce chans-hash chan-keys))
    chan-keys))

(defn close-all-chans! [chans-hash]
  (apply close-chans! chans-hash (keys chans-hash)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(def connected-states
  #{:is-connecting
    :has-connected
    :handling-local-msg
    :handling-remote-msg })

(contains? connected-states :has-connected)

(defn is-connected? [state]
  (contains? connected-states state))

(defn not-connected? [state]
  (not (is-connected? state)))

(not-connected? :has-connected)

(defn add-connection-fsm [this key handler]
  ; (t/info (class this))
  (FSM2/add-fsm this key conn-state-table handler))

(defn remove-connection-fsm [this key]
  (FSM2/remove-fsm this key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-connection-process [event! com-chan kill-chan ws-channel]
  (let [connection (atom {:ws-channel ws-channel
                          :kill-chan kill-chan })]
    (go
      ;; Hunky dory
      (do
        (event! :done {})
        ;; Kill channel - send anything to this an all will close

        (go
          (when-let [msg (<! kill-chan)]
            (println (str "*********killing it! " msg) )
            (event! :kill-chan msg)
            (swap! connection close-all-chans!)))

        ;; handle the any local messages we receive
        (go-loop
          []
          (if-let [msg (<! com-chan)]

            (do ;; yes
                (event! :local-message msg)
                (recur))

            (do ;; no
                (>! kill-chan :com-chan-closed))))

        ;; handle remote message channel
        ;; we're in a go block already

        (loop []
          (if-let [{:keys [error message] :as raw-message} (<! ws-channel)]
            (if error
              (do ;; an error!
                  (event! :remote-socket-error {})
                  (>! kill-chan :remote-socket-error))

              (do ;; all fine
                  (event! :remote-message message)
                  (recur)))

            (do  ;; nil from ws-channel
                (event! :remote-socket-closed {})
                (>! kill-chan :remote-socket-closed))))))))

