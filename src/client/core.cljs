(ns client.core
  (:require

    [shared.component.messagebus :as MB ]
    [shared.component.listeners :as MBL ]
    [shared.component.keystates :as KS]

    [client.component.clock :as CLK]
    [client.audio :as AU]

    [client.game :as game]
    [cljs.analyzer :as ana]
    [taoensso.timbre :as t
      :refer-macros [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]

    [sablono.core :as html :refer-macros [html]]

    [com.stuartsierra.component :as c]

    [shared.protocols.clientconnection :as client]
    [client.client :refer [mk-client-component]]

    [shared.messages :refer [mk-msg]]
    [shared.utils :as su]

    [shared.fsm :as fsm]
    [client.protocols :as p]

    [client.html :refer [mk-html-component]]
    [client.htmlevents :refer [mk-html-events-component]]

    [client.utils :refer [ch->coll cos cos01] :as u]

    [client.keys :as k]

    [goog.dom :as gdom]
    [om.next :as om :refer-macros [defui]]

    [om.dom :as dom]
    [cljs.core.async :refer [chan <! >! put! close! timeout poll!] :as a])

  (:require-macros
    [cljs.core.async.macros :as a :refer [go go-loop]]
    [shared.macros :as m]))


(declare start stop restart ui-log! set-ui-field!)


(def config { :conn-config {:url  "ws://localhost:6502/ws"
                            :ping-frequency 1000 }
             :html-id "game" })

(def ui-listeners
  {:ui-set-field (fn [{:keys [key value]}]
                   (set-ui-field! key value))
   :ui-log #(println (str "logger! " %)) })

(defrecord App [game]

  c/Lifecycle

  (start [this]
    this)

  (stop [this]
    this))

(defn mk-app []
  (map->App {}))

(def app ( mk-app ))

(defn mk-game [config ]

  (let [com-chan (chan) ]

    (c/system-map
      ;; Central message bus
      :messages (MB/mk-message-bus :type)

      ;; listens to the message bus and
      ;; updates UI as and when needed

      :ui-listeners (c/using
                     (MBL/mk-listeners ui-listeners)
                     [:messages])

      :com-chan com-chan

      :config config

      :key-states (KS/mk-keystates)

      :system (mk-html-component (:html-id config))

      :clock (CLK/mk-clock)

      :events (c/using 
                (mk-html-events-component)
                [:key-states
                 :messages
                 :clock])

      :client-connection (c/using
                           (mk-client-component )
                           [:config
                            :com-chan
                            :messages])

      :game (c/using
              (game/mk-game)
              [:client-connection
               :config
               :messages
               :events
               :system
               :key-states ])

      :app (c/using
             (map->App {})
             [:config
              :game
              :ui-listeners]))))

(defonce sys-atom (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def objs (atom []))

(enable-console-print!)

(defn on-js-reload []
  ;; optionally touch your app-state to force re-rendering depending on
  ;; your application
   ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(def app-state (atom {:game-status {:running false
                                    :game-state :stopped
                                    :log [] } }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti reader-fn om/dispatch)

(defmethod reader-fn :default
 [{:keys [state]} k _]
  (let [st @state] ;; CACHING!!!
    (if (contains? st k)
      {:value (get st k)})))

(defmulti mutate-fn om/dispatch)

(defmethod mutate-fn `game/set-field
  [{:keys [state] :as env} key {:keys [field value]}]
  {:action (fn[] (swap! state assoc-in [:game-status field] value))})

(def max-log-entries 10)

(defmethod mutate-fn `game/log
  [{:keys [state] :as env} _ {:keys [payload]}]
  (let [log (-> @state :game-status :log )
        new-log (take max-log-entries (conj log payload)) ]
    {:action (fn[] (swap! state assoc-in [:game-status :log] new-log))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mk-button [this txt action]
  [:button
   {:class  "pure-button pure-button-primary"
    :on-click (fn [] (action))}
   txt])

(defn pad-coll [sz padding coll]
  (take sz
        (concat coll (take sz (repeat padding)))))

(defn log-window [this {:keys [log] :as props}]
  (html (->>
          log
          (pad-coll 10 "")
          (reverse)
          ; (interpose "\n")
          (mapv (fn [v][:p (str v)]))
          (into [:div {:class "log"} ]))))

(defui OmApp

  static om/IQuery

  (query [this] '[:game-status])

  Object

  (render [this]
          (let [{:keys [game-status] :as props} (om/props this)
                {:keys [running game-state]} game-status ]
            (html
              [:div
               [:h1 "H5 test"]

               (log-window this game-status)

               [:p (str "Game state: " (-> game-state name str) )]

               (case game-state
                 :stopped      (mk-button this "Start" start)
                 :running-game (mk-button this "Stop" stop)
                 :waiting      (mk-button this "Stop" stop)
                 :connecting   (mk-button this "Connecting" restart)
                 (mk-button this "" identity)) ]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def reconciler
  (om/reconciler
    {:state app-state
     :parser (om/parser {:read reader-fn :mutate mutate-fn})}))

(defn get-ui-field [k]
  (get (@app-state :game-status ) k ))

(defn set-ui-field! [k v]
  (when (not= ( get-ui-field k ) v)
    (om/transact! reconciler `[(game/set-field {:field ~k :value ~v} )])))

(defn set-ui-game-state! [v]
  (set-ui-field! :game-state v))

(defn ui-log! [v]
 (om/transact! reconciler `[( game/log {:payload ~v} )]) )

(defn main []
  (let [app-el (gdom/getElement "app") ]
    (do
      (om/add-root! reconciler OmApp app-el))))

(defn stop []

 (when @sys-atom
  (c/stop-system @sys-atom)

  (doseq [c [:com-chan]]
    (close! (get @sys-atom c)))

  (reset! sys-atom nil)) )

(defn start []

  (t/info "\n\n\n\n\nRestarting")
  (do
    (t/info "*** stopping ****")
    (stop)
    (t/info "*** stopping done ****\n\n")
    (->>
      (mk-game config )
      (c/start-system )
      (reset! sys-atom))))

(defn restart []
  (start))

(stop)
(main)
