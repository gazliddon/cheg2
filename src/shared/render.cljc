(ns shared.render
  (:require
    [shared.component.messagebus :as MB ]


    [client.sprdata :as sprdata]

    [taoensso.timbre :as t
     :refer-macros [log  trace  debug  info  warn  error  fatal  report
                    logf tracef debugf infof warnf errorf fatalf reportf
                    spy get-env]]


    [clojure.core.async :as a])
  )

;; Platform independant rendering
;; using a canvas abstraction
