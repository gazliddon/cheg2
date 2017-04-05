(ns client.core
  (:require

    [sablono.core :as html :refer-macros [html]]

    [com.stuartsierra.component :as c]

    [servalan.protocols.clientconnection :as client]
    [client.client :as clientcomp]

    [servalan.messages :refer [mk-msg]]

    [servalan.fsm :as fsm]
    [client.protocols :as p] 
    [client.html :as client-html]
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

  (:require-macros 
    [servalan.macros :as m]
    [cljs.core.async.macros :refer [go go-loop]]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def objs (atom []))

(def state (atom {:server-time 0 
                  :conn-status :disconnected
                  :time 0 

                  :pings {:last-ping 0 
                          :last-pong 0    } }))

(enable-console-print!)

;; HTML

;; =============================================================================
(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
   ;; (swap! app-state update-in [:__figwheel_counter] inc)
)


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def app-state (atom {:count 0
                      :time 0
                      :conn-status :disconnected }))

(defmulti reader-fn (fn [env key params] key))

(defmethod reader-fn :time
  [{:keys [state] :as env} key params]
  (:time app-state))

(defmethod reader-fn :conn-status
  [{:keys [state] :as env} key params]
  {:value (str(:conn-status @state))})

(defmethod reader-fn :log
  [{:keys [state] :as env} key params]
  {:value ["log one", "line 12", "line 3"] })

(defmulti mutate om/dispatch)

(def click->next {:disconnected :connecting
                  :connecting :disconnecting
                  :disconnecting :disconnected })

(get click->next :disconnected)

(defmethod mutate 'inc/conn-status
  [{:keys [state]} y { :keys [conn-status] :as z}]
  (println @state)
  (println y)
  (println z)
  {:action
   (fn []
     (swap! state update-in
       [:conn-status] #(get click->next %)))})

(defmethod mutate 'set/conn-status
  [{:keys [state]} action params]
  {:action
   (fn []
     (swap!  state assoc :conn-status (:value params)))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; <button type="submit" class="pure-button pure-button-primary">Sign in</button>

(defn input-template [ ]
  (html
    [:form {:class "pure-form-stacked"} 
     [:fieldset

      [:legend "Login"]

      [:input {:type "text" :placeholder "username":ref "name"}]

      [:input {:type "password" :placeholder "password":ref "password"}]

      [:button {:type "submit" :class "pure-button pure-button-primary"} "Sign In"] ] ]))

(defui LoginBox
  static om/IQuery

  Object
  (query [this]
         [:transaction :show-password])

  (render [this]
          (let [{:keys [transaction] :as props} (om/props this)]
              
            )
          )
  )


(defn login-box [] (om/factory LoginBox))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defui App

  static om/IQuery

  (query [this]
         [:conn-status])

  Object
  (render [this]
          (let [{:keys [conn-status] :as props} (om/props this)]
            (dom/div nil
                     (input-template)

                     (dom/div nil
                              (dom/span nil "connection status ")
                              (dom/span nil conn-status))
                     (dom/button
                       #js {:onClick
                            (fn [e]
                              (comment start-connection))}
                       "Connect")

                     (dom/button
                       #js {:onClick
                            (fn [e]
                              (comment client/disconnect! conn ))}
                       "Disconnect")

                     (dom/button
                       #js {:onClick
                            (fn [e]
                              (om/transact!
                                this
                                `[(inc/conn-status ~props)]))}
                       "Ignore")
                     ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Game [started client-connection system html-events config]
  c/Lifecycle

  (start [this]
    )

  (stop [this]
    )
  )

(def config { :conn-config {:url  "ws://localhost:6502"
                            :ping-frequency 1000 }
             :html-id "game" })

(defn mk-game [config]

  (c/system-map

    :com-chan (chan)

    :config config

    :html-events (client-html/mk-html-events-component)

    :system (client-html/mk-html-component (:html-id config))

    :client-connection (clientcomp/mk-client-component (:conn-config config))

    :game (c/using (map->Game {}) 
                          [:client-connection
                           :html-events
                           :system
                           :config])))

(def reconciler
  (om/reconciler
    {:state app-state
     :parser (om/parser {:read reader-fn :mutate mutate})}))


(defn main []
  (let [app-el (gdom/getElement "app")
        game-el (gdom/getElement "game") ]

    (do
      (om/add-root! reconciler
                    App app-el)
      )
    )
  )

(main)






