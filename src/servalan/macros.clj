(ns servalan.macros
  )

(defmacro dochan [[binding chan] & body]
  `(let [chan# ~chan]
     (clojure.core.async/go
       (loop []
         (if-let [~binding (clojure.core.async/<! chan#)]
           (do
             ~@body
             (recur))
           :done)))))

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
    
    

  

