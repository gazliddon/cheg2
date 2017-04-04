(ns servalan.utils
  (:require #?(:clj [org.httpkit.server :as http])

            #?(:clj
               [clojure.core.async :refer [chan <! >! put! close! go-loop]]
               :cljs
               [cljs.core.async :refer [chan put! close! <! >!]])

            #?(:clj
               [clojure.core.async.impl.protocols :as p]
               :cljs
               [cljs.core.async.impl.protocols :as p]))

  #?(:cljs
     (:require-macros [cljs.core.async.macros :refer [go-loop]])))

(defn bidi-ch

  "bi directional channel"
  
  
  [read-ch write-ch & [{:keys [on-close]}]]
  (reify
    p/ReadPort
    (take! [_ handler]
      (p/take! read-ch handler))

    p/WritePort
    (put! [_ msg handler]
      (p/put! write-ch msg handler))

    p/Channel
    (close! [_]
      (p/close! read-ch)
      (p/close! write-ch)
      (when on-close
        (on-close)))))
