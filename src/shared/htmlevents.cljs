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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn animate! [cbfunc]
  (when (cbfunc)
    (.requestAnimationFrame js/window #(animate! cbfunc))))

(defn vsync-events
  [messages]

  (let [kill-chan (a/chan)
        running? (atom true) ]
    (do
      ;; Close if anything written to me
      (a/go
        (a/<! kill-chan)
        (reset! running? false)
        (a/close! kill-chan))

      ;; put a value onto the channel every
      ;; refresh until we're not running any more
      (animate! (fn []
                  (when @running?
                    (MB/message messages {:type :vsync :payload {}}))))

      kill-chan)))

(defn key-listen-with-callback! [ev func]
  (deaf! ev)
  (->>
    (fn [ev]
      (-> ev (.-event_) (.-key) (keyword) func))
    (events/listen js/window ev )))

(defn mk-input-msg [type payload t]
  {:type :input :payload (mk-msg type payload t) })

(defn set-state! [key-states messages k v]
  (do
    (KS/set-state! key-states k v)
    (let [ks (KS/get-state key-states k) ]
      (MB/message messages (mk-input-msg :key ks 0)))))

(defn add-key-events! [key-states messages]
  (do
    (key-listen-with-callback! keyup-events #(set-state! key-states messages % false) )
    (key-listen-with-callback! keydown-events #(set-state! key-states messages % true) )))

(defn remove-key-events! []
  (do
    (deaf! keyup-events)
    (deaf! keydown-events)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord HtmlEventsComponent [started? messages key-states kill-ch ]
  c/Lifecycle

  (start [this]
    (if-not started?
      (do
        (add-key-events! key-states messages)
        (su/add-members this :started? { :kill-ch (vsync-events messages)})))
    this)

  (stop [this]
    (if started?
      (do
        (a/put! kill-ch :stop)
        (remove-key-events!)
        (su/nil-members this :started?))

      this))
  )

(defn mk-html-events-component []
  (map->HtmlEventsComponent {}))
