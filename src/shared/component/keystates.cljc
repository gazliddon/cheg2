(ns shared.component.keystates
  (:require
    [taoensso.timbre :as t ]
    [com.stuartsierra.component :as c]
    [shared.component.messagebus :as MB]))


(defprotocol IKeyStates
  (clear-states! [_])
  (set-state! [_ k v])
  (get-state [_ k]))

(defrecord KeyStates [started? key-states-a ]

  IKeyStates
  (clear-states! [_]
    (reset! key-states-a {}))

  (set-state! [this k v]
    (let [{:keys [current]}  (get-state this k)]
      (swap! key-states-a assoc k {:current v
                                   :previous current})))

  (get-state [_ k]
    (get @key-states-a k  {:current false
                           :previous false }))
  c/Lifecycle

  (start [this]
    (if-not started?
      (->
        this
        (assoc :key-states-a (atom {})
               :started? true))
      this))

  (stop [this]
    (if started?
      (->
        this
        (assoc :key-states-a nil
               :started? nil))
      this)))

(defn mk-keystates []
  (map->KeyStates {}))
