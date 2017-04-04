(ns client.html
  (:require
    [client.utils :as u]
    [client.protocols :as p] 

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

(defn add-game-html! [div-id]
  (let [game-el (gdom/getElement div-id) 
        wh [100 100]
        html (mk-game-html wh)
        ]
    (do
      (gdom/replaceNode game-el html)
      {:ctx (.getContext (gdom/getElement "canvas") "2d")
       :html html
       :wh wh })))


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
  [
   ;; TODO get events working again

   {:event keydown-events
    :xf (map #(ev->key-event % :keydown)) }

   {:event keyup-events
    :xf (map #(ev->key-event % :keyup)) } 

   {:event resize-events
    :xf (map ev->resizer) }
   
   ] 
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

(defn mk-sys-state [div-id]
  (let [game-html (add-game-html! div-id)
        sys-state (atom nil)

        html-events-ch (map setup-event html-events)
        ev-ch (a/merge html-events-ch)
        to-close (into [p/anim-ch ev-ch] html-events-ch)
        anim-ch (chan (a/dropping-buffer 1))
        stop-animate (u/animate! (fn []
                                 (a/put! anim-ch 
                                          (mk-msg :animate { })) ))
        to-close  (into [p/anim-ch ev-ch] html-events-ch)
        ]

    {:wh (:wh game-html)
     :ctx (:ctx game-html)

     :to-close to-close

     :anim-ch anim-ch

     :hmtl-events-ch html-events

     :ev-ch (a/chan)

     :state :running

     :time {:previous nil :now nil}

     :stop-animate stop-animate

     :stop (fn []
             (let []
               (stop-animate)
               (doseq [ch ()]
                 (a/close! ch)))
             (swap! sys-state assoc :state :stopped))
     }

    ))

(defn add-animation [{:keys [sys-state]}]
  )

(defrecord HtmlSystem []
  

  )

(defn mk-sys-obj []

  (let [sys-state (atom (mk-sys-state "game"))
        get-ctx (fn [] (:ctx @sys-state ))]

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
  (let [sys (mk-sys-obj) ]
    (p/log sys "system made")
    sys))

(comment
  

  )
