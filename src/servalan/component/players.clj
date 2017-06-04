(ns servalan.component.players
  (:require

    [com.stuartsierra.component :as c]

    [shared.component.messagebus :as MB]
    [shared.component.state :as ST]
    [taoensso.timbre :as t ]
    ))

(defn set-player! [this player]
  (ST/set-state! this player))

(defn get-player [this]
  (ST/get-state this))

(defrecord Player [started? state korks connection]

  ST/IStateClient
  (get-korks [_] korks)
  (get-state-holder [_] state)

  c/LifeCycle
  (start [this]
    (if-not started?
      (do
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

(defn mk-player [this id action]
  (let [child-state (ST/child-state this id)
        player (map->Player child-state) ]
    (do
      (ST/set-state! {:action action})
      (c/start player))))


(defn remove-player! [this id]
  (when-let [player (ST/get-state this [id])]
    (c/stop player)
    (ST/set-state! this [id] nil)))

(defn add-player! [this {:keys [send! id] :as msg}]
  (do
    (remove-player! this id)
    (ST/set-state! this [id] (mk-player this id nil))))

(defn stop-all-players! [this]
  (do
    (doseq [p (ST/get-state this)]
      (c/stop p))

    (ST/set-state! this [] nil)
    this))

(defrecord Players [state korks started?]

  ST/IStateClient

  (get-korks [_] korks)
  (get-state-holder [_] state)

  c/Lifecycle

  (start [this]
    (if-not started?
      (->
        this
        (assoc :started? true))
      this))

  (stop [this]
    (if started?
      (->
        this
        (stop-all-players! )
        (assoc :started? nil))
      this)))

(defn mk-players []
  (map->Players {:korks [:players]}))








