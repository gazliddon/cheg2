(ns client.html
  (:require
    [shared.component.keystates :as KS]
    [shared.component.messagebus :as MB]

    [sablono.core :as html :refer-macros [html]]
    [shared.utils :as su]
    [client.utils :as u]
    [com.stuartsierra.component :as c]
    [client.protocols :as p] 

    [goog.dom :as gdom]

    [cljs.core.async :as a])

  (:require-macros 
    [servalan.macros :as m]
    [cljs.core.async.macros :as a ]))

(enable-console-print!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canvas stuff
(defn mk-canvas-element [[ w h ] id]
  (let [e (gdom/createElement "canvas")]
    (do
      (->>
        #js {:width w :height h :id id}
        (gdom/setProperties e ))
      e)))

(defn find-canvas []
 (gdom/getElement "canvas") )

(defn remove-game-html! [game-el]
  ;; TODO only get the canvas in the game el
  (gdom/removeNode (find-canvas)))

(defn ctx-smoothing!
  "set every property we can control how
  scaled images are smoothed"
  [ctx v]
  (do
    (set! (.-imageSmoothingEnabled  ctx) v) 
    (set! (.-mozImageSmoothingEnabled  ctx) v) 
    (set! (.-oImageSmoothingEnabled  ctx) v) 
    (set! (.-webkitImageSmoothingEnabled  ctx) v) 
    (set! (.-msImageSmoothingEnabled  ctx) v)
    nil))

(defn add-game-html! [game-el]

  (let [wh [(.-offsetWidth game-el ) (.-offsetHeight game-el ) ]
        canvas-el (mk-canvas-element wh "canvas")
        ctx (.getContext canvas-el "2d") ]
    (do
      (gdom/removeNode (find-canvas))
      (gdom/appendChild game-el canvas-el)
      (ctx-smoothing! ctx false)

      {:ctx ctx 
       :html canvas-el
       :wh wh })))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handles building html, rendering, logging


(defrecord HtmlComponent [started ctx id wh-atom game-el]
  c/Lifecycle

  (start [this]
    (if-not started
      (let [game-el (gdom/getElement id)
            game-html (add-game-html! game-el)
            init {:started true
                  :ctx (:ctx game-html)
                  :wh-atom (atom (:wh game-html))   
                  :game-el game-el } ]
        (su/add-members :to-nil init))
      this))

  (stop [this]
    (if started
      (do
        (remove-game-html! game-el)
        (su/nil-members this :to-nil))
      this))

  p/IRender

  (resize! [_ wh]
    (reset! wh-atom wh)
    (assert false))

  (dims [_] @wh-atom)

  (set-transform! [this a b c d e f]
    (do
      (.setTransform ctx a b c d e f)
      nil))

  (reset-transform! [this]
    (p/set-transform! this 1 0 0 1 0 0))

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
      (.fillRect ctx x y w h)
      (.restore ctx)
      nil ))

  (clear-all! [this color]
    (do
      (p/reset-transform! this)
      (p/square! this [0 0] (p/dims this) color)))

  p/ILog
  (log [_ v]
    (u/log-js v)) )

(defn mk-html-component [id]
  (map->HtmlComponent {:id id}))



