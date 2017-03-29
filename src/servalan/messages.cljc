(ns servalan.messages
  )

;; Shared message creation etc

(defn mk-msg [typ payload event-time]
  {:type typ :payload payload  :event-time event-time})
