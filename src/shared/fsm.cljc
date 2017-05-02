(ns shared.fsm
  (:require
    [taoensso.timbre :as t ]))

;; Hey it's a state machine!

(defprotocol IStateMachine
  (event! [this ev payload])
  (set-state! [this new-state])
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

  (set-state! [this new-state]
    (reset! state new-state))

  (get-state [this] @state))

(defn mk-state-machine
  ([table dispatcher init-state]
   (->StateMachine table (atom init-state) dispatcher))

 ([table dispatcher]
   (mk-state-machine table dispatcher :none)) )

(defn- get-states
  "get a sequence of unique states in this state table"
  [st]
  (->>
   st
    (vals)
    (mapcat #(into [] %))
    (flatten)
    (distinct)))

(defn add-fsm 
  
  ([this fsm-key table dispatcher init-state]
    (let [fsm-atom (atom nil)]

      (when-not (satisfies? IStateMachine this)
        (t/error (type this) " does not satisfy IStateMachine")
        (assert false))

      (let [new-this (assoc this fsm-key fsm-atom)
            new-fsm (mk-state-machine
                             table
                             (fn [ev payload]
                               (dispatcher new-this ev payload))
                             init-state)]
        (do
          (reset! fsm-atom new-fsm)
          new-this))))

  ([this fsm-key table dispatcher ]
   (add-fsm this fsm-key table dispatcher :none)))

(defn remove-fsm [this fsm-key]
  (assoc this fsm-key nil))
