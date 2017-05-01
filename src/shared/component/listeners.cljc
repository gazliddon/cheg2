(ns shared.component.listeners
  (:require
    [taoensso.timbre :as t ]
    [com.stuartsierra.component :as c]
    [shared.component.messagebus :as MB]

    #?(:clj
       [clojure.core.async :as a ]
       :cljs
       [cljs.core.async :as a ]))
  
  #?(:cljs
     (:require-macros [cljs.core.async.macros :as a ])))


(defn- listen [ch func topic]
  (a/go-loop
    []
    (if-let [msg (a/<! ch)]
      (do
        (func (:payload msg))
        (recur))
      (println (str "topic chan " topic " closed")))))

(defn- mk-listeners! [messages topic-list]
  (->
    (fn [r topic func]
      (let [ch (MB/sub-topic messages topic (a/chan))]
        (do
          (listen ch func topic)
          (assoc r topic ch))))
    (reduce-kv {} topic-list)))

(defn- close-listeners! [listeners]
  (doseq [[_ v] listeners]
    (a/close! v)))

(defrecord Listeners [messages started? listeners to-listen]
  c/Lifecycle

  (start [this]
    (if started?
      this
      (assoc this
             :listeners (mk-listeners! messages to-listen)
             :started? true)))

  (stop [this]
    (if-not (:started? this)
      this
      (do
        (close-listeners! listeners)
        (assoc this
               :listeners nil
               :started? nil)))))

(defn mk-listeners [to-listen]
  (map->Listeners {:to-listen to-listen}))

