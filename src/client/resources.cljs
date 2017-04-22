(ns client.resources
  (:require
    [cljs.core.async :refer [chan <! >! put! close! timeout poll!] :as a] )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))

(defprotocol ICache
  (status? [_ _])
  (add! [_ _ _])
  (remove! [_ _])
  (get-item [_ _]))

(defprotocol ILoader
  (load [_ _]))

(defn mk-cache [loader]
  (let [cache-atom (atom {})]
    (reify
      ICache

      (status? [this id]
        (if-let [{:keys [status] :as v} (-> @cache-atom id)]
          status
          :missing))

      (add! [this id data]
        (let [rec {:status :loaded
                   :data data }]
          (do
            (swap! cache-atom assoc id rec )
            rec)))

      (remove! [this id]
        (swap! cache-atom assoc id nil))

      (get-item [this id]
        (let [ret-ch (chan)]

          (condp (status? this id)

                :loaded (do
                          (put! ret-ch (-> @cache-atom id))
                          ret-ch)

                :loading (println "complex!")

                (let [loader-ch (load loader id)
                      ret-ch (chan)]
                  (do
                    (swap! cache-atom assoc id {:status :loading
                                                :chan  loader-ch })
                    (go
                      (let [data (<! loader-ch)]
                        (if data
                          (do
                            (add! this id data))
                          (do
                            (remove! this id)
                            (>! ret-ch {:errors (str "couldn't load" id) })))))

                    ret-ch))))))))
