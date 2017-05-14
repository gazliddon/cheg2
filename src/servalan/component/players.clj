(ns servalan.component.players
  (:require

    [shared.component.messagebus :as MB]
    [shared.component.state :as ST]

    [taoensso.timbre :as t ]

    [servalan.component.connections :as conns]

    [com.stuartsierra.component :as c]))

(defrecord Player [started? state korks]
  c/LifeCycle

  (start [this]
    (if-not started?
      (do
        (ST/set-state! this [] {})
        (assoc this
               :started? true))
      this))

  (stop [this]
    (if started?
      (do
        (ST/set-state! this [] nil)
        (assoc this
               :started? nil)  )
      this))

  ST/IState

  (get-state [this k]
    (ST/get-state state (into korks k)))

  (set-state [this k v]
    (ST/set-state! state (into korks k) v)))


(defn mk-player [state id action]
  (map->Player {:state state
                :korks [id]
                :action action })
  )

(defprotocol IPlayers
  (add! [_ connection]))

(defrecord Players [state korks started?]

  ST/IState

  (get-state [this k]
    (ST/get-state state (into korks k)))

  (set-state [this k v]
    (ST/set-state! state (into korks k) v))

  IPlayers

  (add! [this id]
    (let [pl (mk-player this id)]
      (do
        (set-state! this [id] pl)
        (c/start pl))))

  c/Lifecycle

  (start [this]
    (if-not started?
      (do
        (ST/set-state! this [] {})
        (assoc this
               :started? true))
      this))

  (stop [this]
    (if started?
      (do
        (ST/set-state! this [] nil)
        (assoc this
               :started? nil)  )
      this)))

(defn mk-players []
  (map->Players {:korks [:players]}))








