(ns shared.msgutils
  (:require
    [shared.component.messagebus :as MB ]))

(defn send-from-nw-msg [messages payload]
  (let [wrapped-msg {:type :from-remote :payload payload}]
    (do
      (MB/message messages wrapped-msg))))


