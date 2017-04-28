(ns shared.fsm
  (:require
    [taoensso.timbre :as t ]))

;; Hey it's a state machine!

(defprotocol IStateMachine
  (event! [this ev payload])
  (get-state [this]))

(defn- event->state [state-table state event]
  (let [st-entry (get state-table event nil)
        next-st (get st-entry state state) ]
    next-st))

(defrecord StateMachine [table state dispatcher]

  IStateMachine

  (event! [this ev payload]
    (let [new-state (->
                      table 
                      (get ev)
                      (get @state ))]
      (when new-state
        (let [old-state @state]
          (reset! state new-state) 
          (dispatcher {:state new-state
                       :old-state old-state
                       :event ev
                       } payload)))
      new-state))

  (get-state [this] @state))

(defn mk-state-machine
  
  ([table dispatcher]
   (->StateMachine table (atom :none) dispatcher)))

(defn- get-states
  "get a sequence of unique states in this state table"
  [st]
  (->>
   st
    (vals)
    (mapcat #(into [] %))
    (flatten)
    (distinct)))

(defn add-fsm [this fsm-key table dispatcher ]
  (let [fsm-atom (atom nil) ]
    (when-not (satisfies? IStateMachine this)
      (t/error (type this) " does not satisfy IStateMachine")
      (assert false))

    (let [new-this (assoc this fsm-key fsm-atom) ]
      (do
        (reset! fsm-atom (mk-state-machine
                           table
                           (fn [ev payload]
                             (dispatcher new-this ev payload))))
        new-this))))

(defn remove-fsm [this fsm-key]
  (assoc this fsm-key nil))
