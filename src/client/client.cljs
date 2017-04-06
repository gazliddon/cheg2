(ns client.client
  (:require 
    [cljs.spec :as s]

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


(defn kill-chans! [ & chans]
  (doseq [c chans]
    (close! c)))

(defn kill-function [state ws-channel kill-chan reason]
  (do
    (println (str "trying closing for : " reason))

    (when (= :connected @state )
      (println (str "actually closing for : " reason))
      (put! ws-channel (mk-msg :disconnect {} 0))
      (kill-chans! ws-channel kill-chan)
      (reset! state :disconnected)) ))

(defonce clock (atom 0))

(defn get-time []
  (swap! clock inc)
  @clock)

(defn run-connection-proccess [state ws-channel kill-chan com-chan ]
  (let [kill-fn (partial kill-function state ws-channel kill-chan)]
    (do
      (reset! state :connected)

      (go
        (<! kill-chan)
        (kill-function state ws-channel kill-chan "kill chan"))

      (chandler
        [{:keys [error] :as msg} ws-channel]

        :on-message (if error
                      (kill-function state ws-channel kill-chan (str "ws channel error: " error ))
                      (do
                        (println (str "got msg -> " msg))
                        (comment handle-msg! (assoc msg :ws-channel ws-channel) ) 
                        ))

        :on-close (kill-function state ws-channel kill-chan "ws channel nil"))

      (go-loop
        []
        (when (= @state :connected)
          (>! com-chan (mk-msg :ping {:hello "sasa"} (get-time)))
          (<! (a/timeout 3000) )
          (recur)))

      (dochan [message com-chan]
              (>! ws-channel message))))) 


(defn make-connection-process
  [url state kill-chan com-chan]

  (let [ch (wsockets/ws-ch url)]
    (go
      (let [{:keys [ws-channel error] :as k} (<! ch)]

        (if error
          (do
            (println "ERROR")
            (reset! state :disconnected))

          (run-connection-proccess state ws-channel kill-chan com-chan))))) 

  nil)

(defrecord ClientComponent [com-chan
                             kill-chan
                             config
                             state]

  client/IClientConnection

  (state? [_]
    @state)

  (disconnect! [this]
    (println "called disconnect!")
    (when ( = @state :connected)
      (put! com-chan (mk-msg :disconnect {} 0))
      (println "disconnecting")
      (reset! state :disconnecting) 
      (a/put! kill-chan :done))
    nil)

  (connect! [this]
    (do
      (println (str "trying to start conneciton to " (:url config)) )

      (when (= :connected (client/state? this))
        (println "disconnecting first")
        (client/disconnect! this))

      (make-connection-process (:url config) state kill-chan com-chan)
      (put! com-chan (mk-msg :ping {} 0))
      )
    )

  c/Lifecycle
  (start [this]
    (if state
      this
      (do
        (assoc this
               :state (atom :disconnected)
               :kill-chan (chan)))))

  (stop [this]
    (if state
      (do
        (put! kill-chan :done)
        (close! kill-chan)
        (assoc this
               :state nil
               :kill-chan nil))
      this)))

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

