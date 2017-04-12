(ns client.html
  (:require
    [shared.utils :as su]
    [client.utils :as u]
    [com.stuartsierra.component :as c]
    [client.protocols :as p] 

    [goog.events :as events]
    [goog.dom :as gdom]

    [hipo.core              :as hipo  :include-macros true]
    ; [dommy.core             :as dommy :include-macros true]   

    [cljs.core.async :refer [chan <! >! put! close! timeout poll!] :as a])

  (:require-macros 
    [servalan.macros :as m]
    [cljs.core.async.macros :refer [go go-loop]])

  )


(enable-console-print!)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canvas stuff

(defn make-canvas-html [[ w h ]]
  ;; TODO Remove this dependency on hipo
  (hipo/create
    [:canvas#canvas {:width w :height h}]))

(defn find-canvas []
 (gdom/getElement "canvas") )

(defn remove-game-html! [game-el]
  ;; TODO only get the canvas in the game el
  (gdom/removeNode (find-canvas))
  )

(defn ctx-smoothing!
  "set every property we can control how
  scaled images are smoothed"
  [ctx v]
  (set! (.-imageSmoothingEnabled  ctx) v) 
  (set! (.-mozImageSmoothingEnabled  ctx) v) 
  (set! (.-oImageSmoothingEnabled  ctx) v) 
  (set! (.-webkitImageSmoothingEnabled  ctx) v) 
  (set! (.-msImageSmoothingEnabled  ctx) v))

(defn add-game-html! [game-el]

  (let [wh [(.-offsetWidth game-el ) 500 ]
        canvas-html (make-canvas-html wh)
        ctx (.getContext canvas-html "2d") ]

    (do
      (gdom/removeNode (find-canvas))
      (gdom/appendChild game-el canvas-html )
      (ctx-smoothing! ctx false)

      {:ctx ctx 
       :html canvas-html
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


(defn animate! [cbfunc]
  (when (cbfunc)
    (.requestAnimationFrame js/window #(animate! cbfunc))))
  
   
(defn mk-animator-channel

  "returns a bi-directional channel
  that sends messages at the refresh rate"

  []

  (let [running? (atom true)

        reader (a/chan (a/dropping-buffer 1))

        writer (chan)

        stop-fn (fn [] (reset! running? false))

        params  {:on-close stop-fn}

        bi-chan (su/bidi-ch reader writer params) ]
    (do

      ;; Close if anything written to me
      (go
        (<! writer)
        (close! bi-chan))

      ;; put a value onto the channel every
      ;; refresh until we're not running any more
      (animate! (fn []
                  (when @running?
                    (put! reader :anim) 
                    true)))

      bi-chan)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord HtmlEventsComponent [started html-events-channels anim-ch ev-ch]
  c/Lifecycle

  (start [this]
    (let [this (c/stop this)
          html-events-channels (map setup-event html-events) ]

      (assoc this
             :started true
             :anim-ch (mk-animator-channel)
             :ev-ch (a/merge html-events-channels)
             :html-events-channels html-events-channels)))

  (stop [this]


    (do
      (when started
        ;; Anim-ch self closes when a value
        (put! anim-ch :stop)

        ;; ev-ch will automatically close after source
        ;; channels closes
        (doseq [c html-events-channels]
          (close! c)) )

      (assoc this
             :started nil
             :html-events-channels nil
             :anim-ch  nil
             :ev-ch nil)))

  p/IEvents

  (anim-ch [_]
    anim-ch)

  (events-ch [_]
    ev-ch))

(defn mk-html-events-component []
  (map->HtmlEventsComponent {}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handles building html, rendering, logging

(defrecord HtmlComponent [started ctx id wh-atom game-el]

  c/Lifecycle

  (start [this]

    (let [this (c/stop this)
          game-el (gdom/getElement id)
          game-html (add-game-html! game-el)]

      (assoc this
             :started true
             :ctx (:ctx game-html)
             :wh-atom (atom (:wh game-html))   
             :game-el game-el)))

  (stop [this]
    (when started
      (remove-game-html! game-el))

    (assoc this
           :started nil
           :ctx nil
           :wh-atom nil
           :game-el nil))

  p/IRender

  (resize! [_ wh]
    (reset! wh-atom wh)
    (assert false))

  (dims [_] @wh-atom)

  (reset-transform! [this]
    (.resetTransform ctx 1 0 0 1 0 0)
    nil)

  (spr! [this rimg [sx sy sw sh] [x y] [w h] ]
    (do
      (.save ctx)
      (.drawImage ctx rimg sx sy sw sh x y w h)
      (.restore ctx)
      nil ))

  (square! [this [x y] [w h] color]
    (do
      (.save ctx)
      (set! (.-fillStyle ctx) (apply u/to-color color))
      (.resetTransform ctx 1 0 0 1 0 0)
      (.fillRect ctx x y w h)
      (.restore ctx)
      )  
    nil) 

  (clear-all! [this color]
    (p/square! this [0 0] (p/dims this) color))

  p/ILog
  (log [_ v]
    (u/log-js v)) 
  )

(defn mk-html-component [id]
  (map->HtmlComponent {:id id}))



