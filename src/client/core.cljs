(ns client.core
  (:require
    [client.game :as game]


    [sablono.core :as html :refer-macros [html]]
    [com.stuartsierra.component :as c]

    [servalan.protocols.clientconnection :as client]
    [client.client :refer [mk-client-component]]

    [servalan.messages :refer [mk-msg]]

    [servalan.fsm :as fsm]
    [client.protocols :as p] 
    [client.html :refer [mk-html-component mk-html-events-component]]
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
    [servalan.macros :as m :refer [dochan]]
    [cljs.core.async.macros :refer [go go-loop]]
    
    ))


(def config { :conn-config {:url  "ws://localhost:6502"
                            :ping-frequency 1000 }
             :html-id "game" })

(defn mk-game [config]

  (c/system-map

    :com-chan (chan)

    :config config

    :system (mk-html-component (:html-id config))

    :events (mk-html-events-component )

    :client-connection (c/using
                        ( mk-client-component (:conn-config config) )
                        [:com-chan]
                        
                        )

    :game (c/using (game/mk-game)
                          [:client-connection
                           :events
                           :system
                           :config])
    )
  )
(defonce sys-atom (atom nil))

(defn stop []
 (when @sys-atom
  (c/stop-system @sys-atom)
  (reset! sys-atom nil)) )


(defn start []
  (do

    (stop)

    (->>
      (mk-game config)
      (c/start-system) 
      (reset! sys-atom ))
    (-> @sys-atom :client-connection client/connect!)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def objs (atom []))

(def state (atom {:server-time 0 
                  :conn-status :disconnected
                  :time 0 

                  :pings {:last-ping 0 
                          :last-pong 0    } }))

(enable-console-print!)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
   ;; (swap! app-state update-in [:__figwheel_counter] inc)
)


;; =============================================================================
;; Objs


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; player


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def app-state (atom {:conn-status :disconnected
                      :game-running false
                      }))


(defmulti reader-fn om/dispatch)

(defmethod reader-fn :default
 [{:keys [state]} k _]
  (let [st @state] ;; CACHING!!!
    (if (contains? st k)
      {:value (get st k)})))


; (defmethod reader-fn :conn-status
;   [{:keys [state] :as env} key params]
;   {:value (str(:conn-status @state))})


; (defmethod reader-fn :game-running
;   [{:keys [state] :as env} key params]
;   {:value (:game-running @state)})


(defmulti mutate-fn om/dispatch)

(defmethod mutate-fn `game/start
  [{:keys [state]} _ params]
  {:action (fn[] (swap! state assoc :game-running true))})

(defmethod mutate-fn `game/stop
  [{:keys [state]} _ params]
  {:action (fn [] (swap! state assoc :game-running false)) })

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

(defn mk-button [this txt transaction action]
  [:button
   {:class  "pure-button pure-button-primary"
    :on-click (fn []
                (action)
                (om/transact! this transaction)) }
   txt]

  ; [:button
  ;  {:class  "pure-button pure-button-primary"

  ;   :on-click (fn []
  ;               (action)
  ;               (om/transact! this transaction)) }
  ;  txt]
  )

(defn game-running-buttons [this]
  )

(defui App

  static om/IQuery

  (query [this]
         '[:game-running])
  Object

  (render [this]
          (let [{:keys [game-running] :as props} (om/props this) ]
            (html
              [:div
               [:div [:h1 "Cheg"]]

               (if game-running

                 [:div
                  [:div (mk-button this "Quit" `[(game/stop)] stop)] 
                  [:div (mk-button this "Connect" `[] #())] ]

                 [:div
                  (mk-button this "Start" `[(game/start)] start) ]) ] ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def reconciler

  (om/reconciler
    {:state app-state
     :parser (om/parser {:read reader-fn :mutate mutate-fn})}))

(defn main []
  (let [app-el (gdom/getElement "app")
        game-el (gdom/getElement "game") ]

    (om/add-root! reconciler
                  App app-el)))

(stop)
(main)






