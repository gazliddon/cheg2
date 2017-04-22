(ns client.game

  (:require
    [shared.fsm :as fsm :refer [add-fsm remove-fsm]]
  
    [client.sprdata :as sprdata]

    [taoensso.timbre :as t
     :refer-macros [log  trace  debug  info  warn  error  fatal  report
                    logf tracef debugf infof warnf errorf fatalf reportf
                    spy get-env]]

    [com.stuartsierra.component :as c]

    [shared.messages :refer [mk-msg]]

    [shared.utils :as su]

    [client.protocols :as p] 

    [client.utils :refer [ch->coll cos cos01 ] :as u]

    [cljs.core.async :refer [chan <! >! put! close! timeout poll!] :as a])

  (:require-macros 
    [servalan.macros :as m :refer [dochan]]
    [cljs.core.async.macros :refer [go go-loop]]))

(def player (atom {:pos [10 10]}))

(def k->dir {:z [-1 0]
             :x [ 1 0]
             :k [0 -1]
             :m [0 1] })

(defn addv2 [[x y] [x1 y1]] [(+ x x1) (+ y y1)])

(defn keys->player [player key-coll]
  (->
    (fn [res {:keys [event keypress]}]
      (if (= event :keydown)
        (let [v (get k->dir keypress [0 0])
              p (addv2 (:pos res) v) ]
          (assoc res :pos p))
        res))
    (reduce player key-coll)))


(comment defn update! [io timer ]
         (let [renderer (renderer io)
               now (/ ( now timer ) 1000    )
               col [(int (* 255 (cos01 (* 10 now )) )) 0 0]
               new-t (mod now 5)
               new-player (keys->player @player (keyboard io)) ]

           (do
             (clear-buffer! renderer  col)
             (draw-objs @objs @t renderer  )
             (square! renderer (:pos new-player) [10 10] [255 255 255] )
             (spr! renderer nil (:pos new-player) [100 100]  )
             (reset! player new-player))))

(defn is-active? [{:keys [start-time duration]} t]
  (and (>= t start-time) (< t (+ start-time duration))))

(defn perc-through [{:keys [start-time duration]} t]
  (/ (- t start-time) duration ))

(defmulti draw-obj (fn [obj t ctx]
                     (:type obj)) )

(defmethod draw-obj :frutz [{:keys [pos col]} t ctx]
  (let [[x y] pos
        new-pos [(+ x (* 100 (cos01 (* 3 t )))) y] ]
    (p/square! ctx new-pos [30 30] col) ))

(defmethod draw-obj :player [{:keys [pos col]} t ctx]
  (p/square! ctx pos [30 30] col))

(defn draw-objs
  "convert these objs into a draw list"
  [ctx objs t]
  (dorun
    (->
      (fn [o]
        (when (is-active? o t)
          (let [my-t (- (:start-time o) t)]
            (draw-obj o my-t ctx) )))
      (map objs))))

(def objs (atom []))



(defn print-waiting! [renderer t]
  (let [red (* 255 (cos01 (*  t 10)))
        col [red 0 255]
        fcol (apply u/to-color col) ]

    (doto renderer
      (p/clear-all! col)
      (draw-objs @objs t)
      ; (p/square! (:pos @player) [10 10] [255 255 255] )
      )))

(defn print! [renderer t]
  (let [spr-bank (sprdata/get-bank :pickups)
      group (sprdata/get-group :pickups :pickups)
      rimg (:img spr-bank)
      scale 2 ]

    (doto renderer
      (p/clear-all! [10 10 10]))

    (doseq [n (range 18 )]
      (let [ypos (-> (+ t (/ n 20))
                     (* 8)
                     cos01
                     (* 30)
                     (+ 10))]
        
      (p/spr! renderer rimg (nth group n) [(* n 16 scale) ypos] [32 32])))))

(defprotocol IGame
  (on-network [_ msg])
  (on-update [_ t])
  (on-message [_ msg] ))

(def game-state-table
  {
   :hello-from-server {:none :starting-game}
   :game-state {:running-game :getting-state}

   :send-to-server {:running-game :sending-to-server}

   :done {:getting-state :running-game
          :sending-to-server :running-game
          :starting-game :running-game
          :stopping-game :none }

   :quit {:running-game :stopping-game}
   })

(defmulti game-state (fn [this ev payload] (:state ev)))

(defmethod game-state :default [_ ev _]
  (t/error "unhandled state entry" (:state ev)))

(defmethod game-state :starting-game
  [{:keys [state ui-chan] :as this} ev payload]

  (put! ui-chan (mk-msg :game-started {} 0))

  (t/info "starting game")
  (swap! state assoc :id 1)
  (fsm/event! this :done {}))

(defmethod game-state :running-game
  [{:keys [state] :as this} ev payload]
  ; (t/info "running game")
  )

(defmethod game-state :stopping-game
  [{:keys [state ui-chan] :as this} ev payload]
  (put! ui-chan (mk-msg :game-stopped {} 0))
  (fsm/event! this :done {}) )

(defmethod game-state :sending-to-server
  [{:keys [state] :as this} ev payload]  
  (fsm/event! this :done {}) )

(defn add-listeners [{:keys [events com-chan] :as this}]
  (let [anim-ch (p/anim-ch events)
        ev-ch (p/events-ch events) ]
    (do

      (t/info "listening for input from com-chan")

      (go-loop
        [] (if-let [msg (<! com-chan) ]
             (do
               (t/info "game handling com-chan msg " msg)
               (on-network this msg)
               (recur))

             (t/info "com-chan closed")))

      (go-loop
        []
        (if-let [msg (<! ev-ch)]
          (do
            (on-message this msg)
            (recur))
          (t/info "ev-ch closed")  ))

      (go-loop
        [t 0]
        (if-let [msg (<! anim-ch)] 
          (do
            (on-update this t)
            (recur (+ t (/ 1 60))))
          (t/info "anim-ch closed")))))
  this)


(defmulti refresh (fn [this _]))

(defmethod refresh :default [this state]
  )

(defmethod refresh :none [this state]
  )

(defrecord Game [started events system com-chan state fsm ui-chan]

  fsm/IStateMachine

  (get-state [this ] (fsm/get-state @fsm))

  (event! [this ev payload]
    (let [new-state (fsm/event! @fsm ev payload)]

      ;; if there's state change then
      ;; tell the ui
      (when new-state
        (put! ui-chan (mk-msg :game-state-change {:state new-state} 0)))

      new-state))

  IGame

  (on-network [this msg]
    (t/info "on-network" msg)
    (t/info "player is " @state)
    (fsm/event! this (:type msg) (:payload msg)))

  (on-update [this t ]
    (do
      ;; send player pos to server
      (let [player-msg 
            (->
              (mk-msg :player @player 0)
              (assoc :id (:id @state)))]

        (fsm/event! this :send-to-server player-msg))

      (case (fsm/get-state this)
        :running-game (print! system t)
        (print-waiting! system t))
      ))

  (on-message [_ msg] )

  c/Lifecycle
  (start [this]

    (t/info "Starting game component")

    (put! ui-chan (mk-msg :game-connecting {} 0))

    (let [ret (-> (c/stop this)
                  (assoc :state (atom {:id -1})
                         :started true )
                  (add-fsm :fsm game-state-table game-state )
                  (add-listeners))]
      ret))

  (stop [this]
    (when started
      (fsm/event! this :quit {})
      (remove-fsm this :fsm))
    (assoc this
           :started nil
           :state nil )))

(defn mk-game[]
  (map->Game {}))




