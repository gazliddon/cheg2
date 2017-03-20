(ns client.core
  ;; Events



  (:require
    [client.protocols :as p] 
    [client.html :as html]
    [client.utils :refer [ch->coll cos cos01] :as u]

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

;; HTML

;; =============================================================================
(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
   ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

;; =============================================================================
;; html bullshit





;; =============================================================================
;; Objs

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
  [objs t ctx]
  (dorun
    (->
      (fn [o]
        (when (is-active? o t)
          (let [my-t (- (:start-time o) t)]
            (draw-obj o my-t ctx) )))
      (map objs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; player

(def player (atom {:pos [10 10]}))

(def k->dir {
   :z [-1 0]
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
      (reset! player new-player))))

(defn make-game []
  (let  [ sys (html/mk-system) ]
    (go
      (loop []
        (let [m (<! (p/events-ch sys))]

          (case (:type m)
            :animate nil
            :resize (println "resize"))
          
         (recur))))
    sys))

(defn main [])







