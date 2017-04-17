(ns shared.mapper
  )


(defprotocol IMap
  (clear [_ v])
  (box [_ xy wh v])
  (hline [_ xy l v])
  (vline [_ xy l v])
  (put-block [_ xy v])
  (get-block [_ xy v]))


(defn in-bounds? [{:keys [wh]} xy]
  (let [[w h] wh
        [x y] xy]

    (and
      (and
        (>= y 0)
        (< y w))

      (and
        (>= x 0)
        (< x w)))))

(defn box-in-bounds? [{ :keys [wh] } [x y] [w h]]
  (let [[mw mh] wh]
   (and
    (>= (+ x w) 0)
    (< x mw)
    (>= (+ y h) 0)
    (< y mh)))
  )

(defn get-index [{:keys [wh]} [x y]]
 (let [[w _] wh ]
   (+ x (* y w))))

(defn co-ords [ [ x y ] [ w h ] ]
  (for [ yy (range y (+ y h))   
        xx (range x (+ x w)) ]
    [ xx yy ]))

(defrecord Map [wh blocks]
  IMap
  (clear [this value]
    (box this [0 0] wh value))

  (hline [this xy len value]
    (box this xy [len 1] value))

  (vline [this xy len value]
    (box this xy [1 len] value) )

  (box [this xy wh value]
    ;; lol
    (if (box-in-bounds? this xy wh)
      (reduce
        (fn [r xy]
          (put-block r xy value))
        this (co-ords xy wh))
      this))

  (put-block [this xy v]
    (if (in-bounds? this xy)
      (assoc this :blocks (assoc blocks (get-index this xy) v))
      this))

  (get-block [this xy v]
    (when (in-bounds? xy)
      (nth blocks (get-index this xy)))))

(defn mk-map [wh v]
  (let [[w h] wh]
    (map->Map {:wh wh
               :blocks (vec (repeat (* w h) v))}))
  )

(comment
  (->
    (mk-map [3 3] :duuno)
    (clear :empty)
    (box [1 1] [1 1] :oof)
    )
  )











