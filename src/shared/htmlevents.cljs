(ns client.htmlevents
  (:require
    [shared.component.keystates :as KS]
    [shared.component.messagebus :as MB]
    [shared.messages :refer [mk-msg]]

    [sablono.core :as html :refer-macros [html]]
    [shared.utils :as su]
    [client.utils :as u]
    [com.stuartsierra.component :as c]
    [client.protocols :as p]

    [goog.events :as events]
    [goog.dom :as gdom]

    [cljs.core.async :as a])

  (:require-macros
    [cljs.core.async.macros :as a ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events

(def keyup-events (.-KEYUP events/EventType))
(def keydown-events (.-KEYDOWN events/EventType))
(def resize-events (.-RESIZE events/EventType))
(def close-window-events (.-BEFOREUNLOAD events/EventType))

(def deaf! events/removeAll)

(defn listen!
  ([ev xf]
   (let [ch (a/chan 1 xf)]
     (events/listen js/window ev #(a/put! ch %))
     ch))
  ([ev]
   (listen! ev (map identity))))

;; listen to a type of key event
(defn ev->key-event [ev id]
  (mk-msg :key {:event id
                :keypress (-> ev
                             (.-event_)
                             (.-key)
                             (keyword)) } 0))

(defn ev->resizer [ev]
  (mk-msg :resize (u/get-win-dims) 0) )

(def html-events
  [
   ; {:event keydown-events
   ;  :xf (map #(ev->key-event % :keydown)) }

   ; {:event keyup-events
   ;  :xf (map #(ev->key-event % :keyup)) }

   {:event resize-events
    :xf (map ev->resizer) } ])

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

        writer (a/chan)

        stop-fn (fn [] (reset! running? false))

        params  {:on-close stop-fn}

        bi-chan (su/bidi-ch reader writer params) ]
    (do

      ;; Close if anything written to me
      (a/go
        (a/<! writer)
        (a/close! bi-chan))

      ;; put a value onto the channel every
      ;; refresh until we're not running any more
      (animate! (fn []
                  (when @running?
                    (a/put! reader :anim)
                    true)))

      bi-chan)))

(defn key-listen-with-callback! [ev func]
  (deaf! ev)
  (->>
    (fn [ev]
      (-> ev (.-event_) (.-key) (keyword) func))
    (events/listen js/window ev )))

(defn mk-input-msg [type payload t]
  {:type :input :payload (mk-msg type payload t) })

(defn set-state! [{:keys [key-states messages] :as this} k v]
  (do
    (KS/set-state! key-states k v)

    (let [ks (KS/get-state key-states k) ]
      (MB/message messages (mk-input-msg :key ks 0)))))

(defn add-key-events! [{:keys [ key-states ] :as this}]
  (do
    (key-listen-with-callback! keyup-events #(set-state! this % false) )
    (key-listen-with-callback! keydown-events #(set-state! this % true) )
    this ))

(defn remove-key-events! [this]
  (do
    (deaf! keyup-events)
    (deaf! keydown-events)
    this ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord HtmlEventsComponent [key-states started html-events-channels anim-ch ev-ch]
  c/Lifecycle

  (start [this]
    (if-not started
      (let [this (c/stop this)
            html-events-channels (map setup-event html-events)
            init  {:started true
                   :anim-ch (mk-animator-channel)
                   :ev-ch (a/merge html-events-channels)
                   :html-events-channels html-events-channels } ]
        (->
          (add-key-events! this)
          (su/add-members :to-nil init)))  
      )
    this)

  (stop [this]
    (if started
      (do
        ;; Anim-ch self closes when a value
        (a/put! anim-ch :stop)

        ;; ev-ch will automatically close after source
        ;; channels closes
        (doseq [c html-events-channels]
          (a/close! c))

        (->
          this
          (remove-key-events!)
          (su/nil-members :to-nil))  
        )
      this))

  p/IEvents

  (anim-ch [_]
    anim-ch)

  (events-ch [_]
    ev-ch))

(defn mk-html-events-component []
  (map->HtmlEventsComponent {}))
