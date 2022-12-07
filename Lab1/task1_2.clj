(defn generateWord [alphabet, word, result]
  (if (empty? alphabet)
    result
     (if (.startsWith word (first alphabet))
      (recur (rest alphabet) word result)
      (recur (rest alphabet) word (cons (str (first alphabet) word) result))
    )
  )
)

(defn generateWords [alphabet, wordsSequence, result]
  (if (empty? wordsSequence)
    result
    (recur alphabet (rest wordsSequence) (generateWord alphabet (first wordsSequence) result))
  )
)

(defn generate [alphabet, wordsSequence, n]
  (if (= n 1)
    wordsSequence
    (recur alphabet (generateWords alphabet wordsSequence '()) (- n 1))
  )
)

(defn wordGeneratorFactory [n]
  (fn [alphabet] (generate alphabet, alphabet, n)))

(let [wordGenerator3 (wordGeneratorFactory 3)]
  (println (wordGenerator3 '("a" "b" "c"))))