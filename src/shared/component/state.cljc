(ns shared.component.state
  (:require
    [com.stuartsierra.component :as c]))

(defprotocol IState
  (get-state [_ _])
  (reset-state! [_ _ _]))

(defrecord State [state-atom started?]

  IState
  (get-state [this korks]
    (get-in @state-atom korks))

  (reset-state! [this korks v]
    (swap! state-atom assoc-in korks v))

  c/Lifecycle

  (start [this]
    (if-not started?
      (assoc this
             :state-atom (atom {})
             :started? true)
      this))

  (stop [this]
    (if started?
      (assoc this
             :started? nil
             :start-atom nil)
      this)))

(defn mk-state []
  (map->State {}))
