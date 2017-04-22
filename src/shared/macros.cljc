(ns shared.macros
  (:require
    [shared.fsm :as fsm]

    [taoensso.timbre :as t
     :refer-macros [log  trace  debug  info  warn  error  fatal  report
                    logf tracef debugf infof warnf errorf fatalf reportf
                    spy get-env]]
    #?(:clj
       [clojure.core.async :as a :refer [chan <! >! put!
                                   close! go-loop alts!
                                   sliding-buffer timeout]]
       :cljs
       [cljs.core.async :as a :refer [chan put! close! <! >! alts! sliding-buffer timeout ]])

    #?(:clj
       [clojure.core.async.impl.protocols :as p]
       :cljs
       [cljs.core.async.impl.protocols :as p]))

  #?(:cljs
     (:require-macros [cljs.core.async.macros :as a :refer [go-loop go]])))
(comment 

 (defmacro dochan [[binding chan] & body]
   `(let [chan# ~chan]
      (~a/go-loop 
        []
        (if-let [~binding (a/<! chan#)]
          (do
            ~@body
            (recur))
          :done))))

 (def chandler-fns #{:on-message :on-close} )

 (defn- check-make-map [clauses ]
   (let [num-clauses (count clauses)
         partitioned (partition 2 clauses) ]

     (assert (even? num-clauses))
     (->
       (fn[r v]
         (assoc r (first v) (second v)))
       (reduce {} partitioned))))

 (defmacro chandler [[binding in-ch] & clauses]
   (let [as-map (check-make-map clauses) ]
     ;; is every even clause a keyword we understand?
     (assert ( every? #(% chandler-fns) (keys as-map) ))

     (let [on-msg (:on-message as-map)
           on-close (:on-close as-map) ]

       `(let [chan# ~in-ch]
          (clojure.core.async/go
            (loop []
              (if-let [~binding (clojure.core.async/<! chan#)]
                (do
                  ~on-msg
                  (recur))
                (do
                  ~on-close))))  
          ))))




 )
