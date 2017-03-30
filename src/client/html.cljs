(ns client.html
  (:require
    [client.utils :as u]
    [client.protocols :refer [IRender ]:as p] 

    [goog.events :as events]
    [goog.dom :as gdom]

    [hipo.core              :as hipo  :include-macros true]
    [dommy.core             :as dommy :include-macros true]   

    [cljs.core.async :refer [chan <! >! put! close! timeout poll!] :as a]))

(enable-console-print!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canvas stuff

(defn mk-game-html [[ w h ]]
  (hipo/create
    [:canvas#canvas {:width w :height h}]))

(defn add-game-html! [wh]
  (let [html (mk-game-html wh)
        game-el (dommy/sel1 "#game") ]
    (do
      (dommy/replace-contents! game-el html)
      {:ctx (.getContext (dommy/sel1 "#canvas") "2d")
       :html html })))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events

(defn mk-msg [typ payload]
  {:type typ
   :payload payload })

(def keyup-events (.-KEYUP events/EventType))
(def keydown-events (.-KEYDOWN events/EventType))
(def resize-events (.-RESIZE events/EventType))
(def close-window-events (.-BEFOREUNLOAD events/EventType))

(def deaf! events/removeAll)

(defn listen!
  ([ev xf] 
   (let [ch (chan 1 xf)]
     (events/listen js/window ev #(put! ch %))
     ch))
  ([ev] 
   (listen! ev (map identity))))

;; listen to a type of key event
(defn ev->key-event [ev id]
  (mk-msg :key
          {:event id
           :keypres (->
                      ev
                      (.-event_)
                      (.-key)
                      (keyword)) }))

(defn ev->resizer [ev]
  (mk-msg :resize (u/get-win-dims)))

(def html-events
  [{:event keydown-events
    :xf (map #(ev->key-event % :keydown)) }

   {:event keyup-events
    :xf (map #(ev->key-event % :keyup)) } 

   {:event resize-events
    :xf (map ev->resizer) } ] 
  )

(defn setup-event [{:keys [event xf]}]
  (deaf! event)
  (listen! event xf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-valid? [{:keys [previous now] :as c}]
  (not (or (nil? previous) (nil? now))))

(defn time-passed [{:keys [previous now] :as c}]
  (if (is-valid? c)
    (- now previous)
    0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn animate-sys
  "add .requestAnimationFrame events"
  [sys]
  (u/animate! (fn []
                (a/put!
                  (p/anim-ch sys)
                  (mk-msg :animate (p/now sys))))))

(defn mk-sys-state-atom [im-wh]
  (let [sys-atom (atom nil)
        html-events-ch (map setup-event html-events)
        ev-ch (a/merge html-events )
        to-close (into [anim-ch ev-ch] html-events-ch) ]

    (reset!
      sys-atom
      {:wh im-wh

       :to-close (into [anim-ch ev-ch] html-events-ch)

       :anim-ch (chan (a/dropping-buffer 1))

       :hmtl-events-ch html-events

       :ev-ch ev-ch

       :state :running

       :time {:previous nil :now nil}

       :ctx (:ctx (add-game-html! im-wh))

       :stop-animate (u/animate! (fn []
                                   (comment a/put! (:anim-ch @sys-atom) 
                                            (mk-msg :animate { })) )) 

       :stop (fn []
               (let [stop-anim (-> :stop-animate @sys-atom)]
                 (stop-anim)
                 (doseq [ch (:to-close @sys-atom)]
                   (a/close! ch)))
                 (swap! sys-atom assoc :state :stopped))
       })

    sys-atom))

(defn mk-sys-obj [sys-state]

  (let [get-ctx (fn [] (:ctx @sys-state ))]

    (reify
      p/IService

      (start! [this])

      (stop! [this]
        ( (-> :stop @sys-state) )
        (swap! sys-state assoc :state :stopped))

      (running? [this]
        (not= (p/state this) :stopped))

      (state [_]
        (-> :state @sys-state ))

      p/ITimer

      (from-seconds [this s] (+ (* 1000 s) ))

      (now [_] (.now (aget js/window "performance")))

      (tick! [this]
        (do
          (let [now (p/now this)]
            (->>
              (swap! sys-state assoc :time {
                                            :previous (-> :time :now @sys-state)
                                            :now (p/now this) }))
            (time-passed (:time @sys-state)))))

      p/IEvents

      (anim-ch [_] (-> :anim-ch @sys-state ))
      (events-ch [_] (-> :ev-ch @sys-state))

      p/IRender
      (resize! [_ wh] (swap! sys-state assoc :wh wh ))

      (dims [_] (-> :wh @sys-state))

      (reset-transform! [this]
        (.resetTransform (get-ctx) 1 0 0 1 0 0)

        this)

      (square! [this [x y] [w h] color]
        (let [ctx (get-ctx)]
          (.save ctx)
          (set! (.-fillStyle ctx) (apply u/to-color color))
          (.resetTransform ctx 1 0 0 1 0 0)
          (.fillRect ctx x y w h)
          (.restore ctx))  
        this) 

      (clear-all! [this color]
        (p/square! this [0 0] (p/dims this) color))

      p/ILog
      (log [_ v]
        (u/log-js v)))
    ))

(defn mk-system []
  (let [sys-state (mk-sys-state-atom (u/get-win-dims))
        sys (mk-sys-obj sys-state) ]
    (p/log sys "system made")
    sys))

(comment

  

  )
