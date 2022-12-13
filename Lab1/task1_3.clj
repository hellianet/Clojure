(defn my-map [f, coll]
  (reverse (reduce (fn [acc, item](conj acc (f item)))
    '() coll)))

(defn my-filter [pred, coll]
  (reverse (reduce (fn [acc item]
    (if (pred item)(conj acc item)acc))
    '() coll)))

(println (my-map (fn [x] (* x 2)) (range 10)) )
(println (my-filter (fn [x] (> x 5)) (range 10)) )