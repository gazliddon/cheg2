(ns client.core
  (:require
    [cljs.core.async :refer [chan <! >! put! close! timeout]]
    [chord.client :refer [ws-ch]]
    [hipo.core              :as hipo  :include-macros true]
    [dommy.core             :as dommy :include-macros true]  )
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)


(defn conn []
  (go
    (let [{:keys [ws-channel error] :as k} (<! (ws-ch "ws://localhost:6502"))]
      (if error
        (println (str "Error " error))
        (do
          (>! ws-channel "player joined")
          (loop []
            (let [msg (<! ws-channel)]
              (when msg
                (println (str "from server: " msg))
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


(defn id-ize   [v] (str "#" v))
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
  (let [new-el (hipo/create [:canvas#canvas {:width width :height height}])]
    (.appendChild el new-el)))


(defn clear-canvas!
  "Clears the canvas"
  [ctx width height color]
  (.save ctx)
  (set! (.-fillStyle ctx) (apply to-color color))
  (.resetTransform ctx 1 0 0 1 0 0)
  (.fillRect ctx 0 0 width height)
  (.restore ctx))

;;; }}}


(defprotocol IRender
  (reset [this])
  (clear [this col])
  (square [this xy wh col]))

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
    (square ctx new-pos [30 30] col) ))

(def objs
  [{:type :frutz
    :start-time 1.5
    :duration 5
    :pos [10 10]
    :col [255 0 0] } 

   {:type :frutz
    :start-time 0
    :duration 5
    :pos [100 30]
    :col [255 0 255]}])

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

  (reset [this]
    (.resetTransform ctx 1 0 0 1 0 0))

  (clear [this color]
    (square this [0 0] [w h] color))

  (square [this [x y] [w h] color]
    (.save ctx)
    (set! (.-fillStyle ctx) (apply to-color color))
    (.resetTransform ctx 1 0 0 1 0 0)
    (.fillRect ctx x y w h)
    (.restore ctx)  
    this) )

(defn mk-canvas [ctx w h]
  (->Canvas ctx w h))


(def x (atom 0))

(def t (atom 0))

(defn update-time! [dt]
  (let [new-t (mod (+ dt @t) 5) ]
    (reset! t new-t)))


(defn update! [ctx timer]
  (let [now (/ ( now timer ) 1000    )
        col [(int (* 30 (cos01 now) )) 0 0]
        new-t (mod now 5)]
    (doall
      (clear ctx col)
      (draw-objs objs new-t ctx ))))

(defn main []
  (let [w 400
        h 400
        gdiv (dommy/sel1 "#game")  
        canvas (add-canvas! "#canvas" gdiv w h)
        ctx   (.getContext (dommy/sel1 "#canvas") "2d")
        inf (mk-canvas ctx w h) ]
    (animate! (fn [timer]
                (update! inf timer)
                )))
  )

(main)


