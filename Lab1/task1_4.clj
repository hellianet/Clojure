(defn my-map [f, coll]
  (reverse (reduce (fn [acc, item](conj acc (f item)))
    '() coll)))

(defn my-filter [pred, coll]
  (reverse (reduce (fn [acc item]
    (if (pred item)(conj acc item)acc))
    '() coll)))

(defn generateWord [wordList, alphabet]
  (reduce (fn [acc, item]
    (concat item acc))
  '()
  (my-map
    (fn [word]
      (my-map
        (fn [x]
          (cons x word))
        (my-filter (fn [letter]
                     (not (= letter (first word))))
                   alphabet)))
    wordList)))

(defn generate [alphabet, n]
  (reduce (fn [acc i]
    (generateWord acc alphabet))
      '(()) (range n)))

(defn wordGeneratorFactory [n]
  (fn [alphabet] (generate alphabet, n)))

(let [wordGenerator3 (wordGeneratorFactory 3)]
  (println (wordGenerator3 '(a b c d))))