(ns shared.component.messagebus
  (:require
    [taoensso.timbre :as t ]


    [com.stuartsierra.component :as c]

    [clojure.pprint :as pp]

    #?(:clj
       [clojure.core.async :as a :refer [chan <! >! put!
                                         close! go-loop alts!
                                         sliding-buffer timeout go]]
       :cljs
       [cljs.core.async :as a :refer [chan
                                      put!
                                      close! <! >!
                                      alts! sliding-buffer timeout]]))
  
  #?(:cljs
     (:require-macros [cljs.core.async.macros :as a :refer [go go-loop]])))

(defprotocol IMessageBus
  (message [this msg])
  (sub-all [this ch]) 
  (sub-topic [this topic ch]))

(defrecord MessageBus [topic-fn xf
                       tap-chan
                       pub-chan
                       mult
                       pub-bus
                       ]
   c/Lifecycle

   (start [this]
     (let [tap-chan (chan 1 xf)
           pub-chan (chan 1 xf)
           mult (a/mult tap-chan)
           pub-bus (a/pub pub-chan :type) ]
       (->
         (c/stop this)
         (assoc :pub-chan pub-chan
                :tap-chan tap-chan
                :mult mult
                :pub-bus pub-bus))))

   (stop [this]
     (when pub-chan
       (a/close! tap-chan)
       (a/close! pub-chan))
     (-> this
         (assoc :pub-chan nil
                :tap-chan nil
                :mult nil
                :pub-bus nil)))

   IMessageBus

   (message [this msg]
     (do
       (a/put! tap-chan msg)
       (a/put! pub-chan msg) 
       this))

   (sub-all [this ch]
     (do
       (a/tap mult ch) 
       ch))

   (sub-topic [this msg-type ch]
     (do
       (a/sub pub-bus msg-type ch)  
       ch)))

(defn mk-message-bus
  ([topic-fn xf]
   (map->MessageBus {:topic-fn topic-fn
                     :xf xf }))

  ([topic-fn]
   (mk-message-bus topic-fn (map identity))) )


