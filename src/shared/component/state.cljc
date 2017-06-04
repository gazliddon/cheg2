(ns shared.component.state
  (:require
    [com.stuartsierra.component :as c]))

(defprotocol IState
  (get-state-atom [_])
  )
(defprotocol IStateClient
  (get-korks [_])
  (get-state-holder [_]))

(defrecord State [state-atom started?]
  IState

  (get-state-atom [this]
    state-atom)

  c/Lifecycle

  (start [this]
    (if-not started?
      (assoc this
             :started? true)
      this))

  (stop [this]
    (if started?
      (assoc this
             :started? nil
             :start-atom nil)
      this)))

(defn mk-state
  ([at]
   (map->State {:state-atom at}) )
  ([]
   (mk-state (atom {}))))

(defn- get-and-check-state-holder
  [this]
  (do
    (assert (satisfies? IStateClient this))
    (let [st (get-state-holder this) ]
      (do
        (assert (satisfies? IState st))
        st))))

(defn- get-and-check-state-atom
  [this]
  (get-state-atom (get-and-check-state-holder this)))

(defn child-state [this & korks]
  {:state (get-and-check-state-holder this)
   :korks (into (get-korks this) korks)})

(defn get-state
  ([this korks]
   (let [sa (get-and-check-state-atom this)
         full-korks (into (get-korks this) korks) ]
     (get-in @sa full-korks)))

  ([this]
   (let [sa (get-and-check-state-atom this) ]
     (get-in @sa (get-korks this)))))

(defn set-state!
  ([this korks v]
   (let [sa (get-and-check-state-atom this)
         full-korks (into (get-korks this) korks) ]
     (do
       (swap! sa assoc-in full-korks v)
       v)))

  ([this v]
   (set-state! this [] v)))

