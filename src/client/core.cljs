(ns client.core

  (:require

    [client.keys :as k]

    [goog.events :as events]

    [goog.dom :as gdom]
    [om.next :as om :refer-macros [defui]]
    [om.dom :as dom] 


    [cljs.core.async :refer [chan <! >! put! close! timeout poll!] :as a]
    [chord.client :refer [ws-ch]]
    [hipo.core              :as hipo  :include-macros true]
    [dommy.core             :as dommy :include-macros true]  )

  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(def objs (atom []))

(def t (atom 0))

(enable-console-print!)

(defn mk-player-msg [typ payload event-time]
  {:type typ :payload payload  :event-time event-time})


(defmulti handle-msg! :type)

(defmethod handle-msg! :objs [msg]
  (reset! objs (:payload msg)))


(defmethod handle-msg! :time [msg]
  (reset! t (:event-time msg)))

(defn conn []
  (go
    (let [{:keys [ws-channel error] :as k} (<! (ws-ch "ws://localhost:6502"))]

      (if error

        (println (str "error: " error))

        (do
          (println (str "connected to server " k))

          (>! ws-channel (mk-player-msg :joined "I joined!" 0))

          (loop []
            (let [{:keys [message event-time] :as msg} (<! ws-channel)]
              (when msg
                (handle-msg! message)
                (recur)))))
        ))))

(conn)

;; =============================================================================
;; {{{ hey! maths!
(defn cos [v]
 (.cos js/Math v) )

(defn cos01 [v]
  (/ (+ 1 (cos v) ) 2  ) )

;;; }}}
;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world2 !"}))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
   (swap! app-state update-in [:__figwheel_counter] inc)
)

(defn id-ize  [v] (str "#" v))

(defn by-id
  ( [v] (-> (id-ize v) (dommy/sel1)) )
  ( [v b] (->> (id-ize v) (dommy/sel1 b)) )
  
  )
(defn get-dims [e] (mapv e [:width :height]))
(defn log-js   [v] (.log js/console v))

(defn to-color [& rgbas]
  (let [csv (apply str (interpose ", " rgbas))]
    (str "rgb(" csv ")")) )

;; =============================================================================
;; {{{ Timer
(defprotocol ITimer
  (now [_])
  (from-seconds [_ s])
  (tick! [_]))

(defn is-valid? [{:keys [previous now] :as c}]
  (not (or (nil? previous) (nil? now))))

(defn time-passed [{:keys [previous now] :as c}]
  (if (is-valid? c)
    (- now previous)
    0))

(defn html-timer []
  (let [c (atom {:previous nil :now nil}) ]
    (reify ITimer

      (from-seconds [this s] (+ (* 1000 s) ))
      (now [_] (.now (aget js/window "performance")))
      (tick! [this]
        (do
          (->>
            (assoc @c
                   :previous (:now @c)
                   :now (now this))
            (reset! c)
            (time-passed))
          )))))

;; }}}

;; =============================================================================
;; {{{ Animator with Timer
(def my-timer (html-timer))

(defn animate! [callback-fn]
  (do
    (.requestAnimationFrame js/window #(animate! callback-fn))
    (let [dt (tick! my-timer)]
      (when (not= dt 0)
        (callback-fn my-timer)))))

;;; }}}

;; =============================================================================
;; {{{ Canvas BS
(defn add-canvas! [id el width height]
  (let [new-el (hipo/create [:canvas#canvas
                             {:style {:position "absolute"
                                      :left "0px"
                                      :right "0px"}
                              :width width
                              :height height}])]
    (.appendChild el new-el)))

;;; }}}


(defprotocol IRender
  (dims [_])
  (reset-transform! [this])
  (clear-buffer! [this col])
  (square! [this xy wh col]))

;; =============================================================================

;; Event -> channel stuff

(def deaf! events/removeAll)

(defn listen!
  ([ev xf] 
   (let [ch (chan 1 xf)]
     (events/listen (.-body js/document) ev #(put! ch %))
     ch))
  ([ev] 
   (listen! ev (map identity))))

;; Now keyboard events
(def keyup-events (.-KEYUP events/EventType))
(def keydown-events (.-KEYDOWN events/EventType))

(defn to-key [ev]
  (->
    ev
    (.-event_)
    (.-key)
    (keyword)))

;; listen to a type of key event

(defn listen-key-ev![ev-type id]
  (listen! ev-type (comp
                     (map #({:event id
                             :key (to-key %) }))
                     (dedupe))))

;; Listen to up / down events
;; and merge them into one stream
(defn deaf-keys! []
  (deaf! (.-body js/document) keydown-events)
  (deaf! (.-body js/document) keyup-events))

(defn listen-keys! []
  (do
    (deaf-keys!)
    (a/merge [(listen-key-ev! keydown-events :keydown) 
              (listen-key-ev! keyup-events :keyup)])))

;; =============================================================================

(defn is-active? [{:keys [start-time duration]} t]
  (and (>= t start-time) (< t (+ start-time duration))))

(defn perc-through [{:keys [start-time duration]} t]
  (/ (- t start-time) duration ))

(defmulti draw-obj (fn [obj t ctx]
                     (:type obj)) )

(defmethod draw-obj :frutz [{:keys [pos col]} t ctx]
  (let [[x y] pos
        new-pos [(+ x (* 100 (cos01 (* 3 t )))) y] ]
    (square! ctx new-pos [30 30] col) ))

(defmethod draw-obj :player [{:keys [pos col]} t ctx]
  (square! ctx pos [30 30] col))

(defn draw-objs
  "convert these objs into a draw list"
  [objs t ctx]
  (dorun
    (->
      (fn [o]
        (when (is-active? o t)
          (let [my-t (- (:start-time o) t)]
            (draw-obj o my-t ctx) )))
      (map objs))))


(defrecord Canvas [ctx w h]
  IRender
  (dims [_]
    [w h])

  (reset-transform! [this]
    (.resetTransform ctx 1 0 0 1 0 0))

  (clear-buffer! [this color]
    (square! this [0 0] [w h] color))

  (square! [this [x y] [w h] color]
    (.save ctx)
    (set! (.-fillStyle ctx) (apply to-color color))
    (.resetTransform ctx 1 0 0 1 0 0)
    (.fillRect ctx x y w h)
    (.restore ctx)  
    this) )


(defn update-time! [dt]
  (let [new-t (mod (+ dt @t) 5) ]
    (reset! t new-t))) 

(defn ch->coll [ch]
  (loop [coll nil]
    (let [v (a/poll! ch)]
      (if-not v
        coll
        (recur (conj coll v))))))

(defprotocol IIo
  (renderer [this])
  (keyboard [this])
  (timer [this]))

(defprotocol IGameLoop
  )

(defn get-win-dims []
  [(.-innerWidth js/window)  
   (.-innerHeight js/window)  ])

(defn mk-html-io 
  ([id w h]
   (do
     (let [gdiv (dommy/sel1 id)
           canvas (add-canvas! "#canvas" gdiv w h) 
           ctx   (.getContext (dommy/sel1 "#canvas") "2d")
           r (->Canvas ctx w h)
           key-ch (listen-keys!)]

       (reify
         IGameLoop

         IIo
         (renderer [this]
           r)

         (keyboard [this]
           (ch->coll key-ch))

         (timer [this]
           0))))
   ) 
  ([id]
   (let [[w h] (get-win-dims)]
     (mk-html-io id w h)))
  
  )

(def player (atom {:pos [10 10]}))

(def k->dir {
   :z [-1 0]
   :x [ 1 0]
   :k [0 -1]
   :m [0 1] })

(defn addv2 [[x y] [x1 y1]] [(+ x x1) (+ y y1)])

(defn keys->player [player key-coll]
  (->
    (fn [res {:keys [event key]}]
      (if (= event :keydown)
        (let [v (get k->dir key [0 0])
              p (addv2 (:pos res) v) ]
          (assoc res :pos p))
        res))
    (reduce player key-coll)))

(defn update! [io timer ]
  (let [renderer (renderer io)
        now (/ ( now timer ) 1000    )
        col [(int (* 255 (cos01 (* 10 now )) )) 0 0]
        new-t (mod now 5)
        new-player (keys->player @player (keyboard io)) ]
    (do
      (clear-buffer! renderer  col)
      (draw-objs @objs @t renderer  )
      (square! renderer (:pos new-player) [10 10] [255 255 255] )
      (reset! player new-player))))

(defn main []
  (do
    (let [io (mk-html-io "#game")]
      (animate!
        (fn [timer]
          (update! io timer)))
      )
    )
  )

(main)

