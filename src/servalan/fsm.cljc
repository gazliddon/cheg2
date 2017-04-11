(ns servalan.fsm
  (:require
    [clojure.pprint :as pp :refer [pprint]] 
    )
  )

;; Hey it's a state machine!

(defprotocol IStateMachine
  (event! [this ev payload])
  (get-state [this]))

(defn event->state [state-table state event]
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
   (->StateMachine table (atom :none) dispatcher))

  ([table]
   (mk-state-machine table (fn [a b] nil))))

(defn get-states
  "get a sequence of unique states in this state table"
  [st]
  (->>
    st
    (vals)
    (mapcat #(into [] %))
    (flatten)
    (distinct)))

 
