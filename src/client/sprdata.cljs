(ns client.sprdata
  (:require
    [goog.dom :as gdom]))

(defn mk-spr-grid-fn
  [base-x base-y cell-w cell-h]

  (let [a2 (fn [x y] [(+ base-x (* x cell-w))
                        (+ base-y (* y cell-h))
                        cell-w
                        cell-h ])


        a4 (fn [x y w h]
             (-> (fn [n]
                   (a2 (+ x (mod n w))
                       (+ y (int (/ n h)))))
                 (mapv (range (* w h)))))
        ]
    (fn
      ([x y] (a2 x y)) 
      ([x y w h] (a4 x y w h)))))


(defn spr16
  ([x y w h] (mapv #(* 16 %) [x y w h]))
  ([x y] (spr16 x y 1 1))
  ([n] (spr16 (mod n 16) (int (/ n 16)))))


(defn spr32
  ([x y] (mapv #(* 32 %) [x y 1 1])))

(defprotocol IGrabber
  (grab-single [_ xy] )
  (grab-range [_ xywh] ))

(defn mk-grabber [[ base-x base-y ] [ cell-width cell-height ]]
  (reify
    IGrabber

    (grab-single [_ [x y]]
      [ (+ base-x (* x cell-width))
       (+ base-y (* y cell-width))
       cell-width
       cell-height ])

    (grab-range [this [x y w h]]
      (->
        (fn [n]
          (grab-single
            this
            [(mod n w)
             (int (/ n w)) ]))
        (mapv (range (* w h)))))))

(defn grab-font [{:keys [grab sprs]}]
  (let [{:keys [grid-base grid-cell-dims]} grab
        grab-gen (mk-grabber grid-base grid-cell-dims )]
    (->
      (fn [res k v]
        (assoc res k
               (mapcat #(grab-range grab-gen %) v)))
      (reduce-kv {} sprs))))

(defn mk-html-sprs [{:keys [img] :as original}]
  {:sprs (grab-font original)
   :img (gdom/getElement (name img))
   :original original })

(def big-font {:img :robey

               :grab {:grid-base [0 226]
                      :grid-cell-dims [8 8] }

               :sprs {:caps [[0 0 18 1]
                             [0 1  8 1]]
                      :nums [[0 1 9 10]] }
               })

(def pickups {:img :robey
              :grab {:grid-base [0 263]
                     :grid-cell-dims [16 16] }

              :sprs {:pickups [[0 0 18 1] ] } })

(def sprs {:big-font big-font
           :pickups   pickups })

(defn mk-spr-data []
  (->
    (fn [res k v]
      (println k v)
      (assoc res k (mk-html-sprs v)))
    (reduce-kv {} sprs) ))


(def spr-data (mk-spr-data))

(defn get-frame [spr-bank group n]
  (-> spr-bank :sprs group (nth n)))

(defn get-bank [bank]
  (-> spr-data bank))

(defn get-group [bank group]
  (-> spr-data bank :sprs group))

(defn get-spr [bank group n]
  (let [box (-> spr-data bank :sprs group (nth n) )]
    {:img (-> spr-data bank :img)
     :spr box }))






