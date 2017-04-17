(ns shared.connhelpers
  (:require
    [servalan.fsm :as fsm]

    #?(:clj [org.httpkit.server :as http])

    #?(:clj
       [clojure.core.async :as a :refer [chan <! >! put!
                                   close! go-loop alts!
                                   sliding-buffer timeout]]
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
  "close any of the chans in the vec of chah-keys
  return a hash with closed channels removed"

  [chans-hash & chan-keys]

  (println (str "closing " chan-keys))

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

(defn create-connection-process [event! com-chan kill-chan ws-channel]

  (let [connection (atom {:ws-channel ws-channel
                          :kill-chan kill-chan })]
    (go
      ;; Hunky dory
      (do
        (event! :connect {}) 

        ;; Kill channel - send anything to this an all will close

        (go
          (when-let [msg (<! kill-chan)]
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
                (>! kill-chan :remote-socket-closed))))))

    connection))

