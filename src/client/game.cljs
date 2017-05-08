(ns client.game

  (:require
    [shared.component.keystates :as KS]

    [shared.protocols.clientconnection :as client]
    [shared.fsm :as fsm :refer [add-fsm remove-fsm]]
    [shared.component.messagebus :as MB ]

    [goog.dom :as gdom]

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
    [cljs.core.async.macros :as a :refer [go go-loop]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO put in own file - message bus

(defn topic->func! [{:keys [messages] :as this} topic cb]
  (let [sub (MB/sub-topic messages topic (chan))]
      (t/info "listening for topic " topic)
   (go-loop
        []
        (if-let [msg (<! sub)]
          (do
            (cb msg)
            (recur))
          (t/info topic " listener closed")))))

(defn topic->event! [this topic ]
  (topic->func! this topic #(fsm/event! this topic (:payload %))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        [w h] (p/dims renderer)
        desired 320
        scale (/ w desired) ]
    (do
      (p/clear-all! renderer [10 10 10])

      (p/set-transform! renderer scale 0 0 scale 0 0)

      (let [t (* t 1.001)]
        (doseq [n (range 28 )]
          (let [ypos (-> (+ t (/ n 20))
                         (* 8)
                         cos01
                         (* 30)
                         (+ 50))]
            (p/spr! renderer rimg (nth group n) [(* n 16) ypos] [16 16])))  )


      (doseq [n (range 28 )]
        (let [ypos (-> (+ t (/ n 20))
                       (* 8)
                       cos01
                       (* 30)
                       (+ 10))]

          (p/spr! renderer rimg (nth group n) [(* n 16) ypos] [16 16]))) 
      )))

(def game-state-table {:hello-from-server {:waiting :starting-game}

                       :network-error {:running-game :doing-network-error}

                       :vsync {:running-gane :updating-game
                               :waiting :updating-waiting }

                       :done {:starting-game :running-game
                              :stopping-game :not-running
                              :doing-network-error :not-running

                              :updating-game :running-game
                              :updating-waiting :waiting }

                       :quit {:running-game :stopping-game} })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mk-to-remote-msg [type payload t]
  {:type :to-remote :payload (mk-msg type payload t) })

(defn send-msg-to-remote [{:keys [messages] } typ payload]
  (let [msg (mk-to-remote-msg typ payload 0)]
      (MB/message messages msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handling remote messages
(defmulti on-remote-message (fn [this msg] (:type msg)))

(defmethod on-remote-message :ping [this msg]
    (send-msg-to-remote this :pong (:payload msg)))

(defmethod on-remote-message :default [this msg]
  (t/error "unhandled remote message" msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handling system messags

(defmulti  on-system-message (fn [this msg] (:type msg)) )

(defmethod on-system-message :vsync [this msg]
  (fsm/event! this :vsync {}))

(defmethod on-system-message :key [this msg]
  ;; TODO something with keys here, probably add to a Q
  )

(defmethod on-system-message :default [this msg]
  (t/error "unhandled system message" msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def not-running-states #{:not-running })

(defn- is-not-running? [this]
  (contains?  not-running-states (fsm/get-state this )))

(defn- is-running? [this] (not (is-not-running? this)))

(defmulti game-state (fn [this ev payload] (:state ev)))

(defmethod game-state :default [_ ev _]
  (t/error "unhandled state entry" (:state ev)))

(defmethod game-state :starting-game
  [{:keys [state ] :as this} ev payload]
  (swap! state assoc :id (-> payload :payload :id))
  (fsm/event! this :done {}))

(defmethod game-state :stopping-game
  [{:keys [state ] :as this} ev payload]
  (fsm/event! this :done {}))

(defmethod game-state :running-game
  [{:keys [state ] :as this} ev payload])

(defn add-listeners [{:keys [events com-chan messages] :as this}]
  (do
    (topic->event! this :from-remote)
    (topic->event! this :system )
    this))

(defn ui-set-field! [{:keys [messages] :as this} k v]
 (let [msg {:type :ui-set-field :payload {:key k :value v }}]
    (MB/message messages msg )))

(defrecord Game [started? events system com-chan state fsm messages
                 client-connection ]

  fsm/IStateMachine
  (get-state [this] (fsm/get-state @fsm))
  (event! [this ev payload] (fsm/event! @fsm ev payload))

  c/Lifecycle

  (start [this]
    (if-not started?
      (let [this (-> this 
                     (assoc :started? true
                            :state (atom nil))
                     (add-fsm :fsm game-state-table game-state :waiting )
                     (add-listeners))]
        (do
          (client/connect! client-connection)
          this)))

    this)

  (stop [this]
    (if started?
      (do
        (->
          this
          (fsm/event! :quit {})
          (remove-fsm :fsm) 
          (assoc :started nil
                 :state nil)))
      this))
  
  )

(defn mk-game[]
  (map->Game {}))




