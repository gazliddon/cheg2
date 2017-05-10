(ns shared.component.keystates
  (:require
    [shared.utils :as su]
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
      (su/add-members this
                      :started?
                      { :key-states-a (atom {}) })
      this))

  (stop [this]
    (if started?
      (su/nil-members this :started?)
      this)))

(defn mk-keystates []
  (map->KeyStates {}))
