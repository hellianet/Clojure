(defn generateWord [alphabet, word, result]
  (if (empty? alphabet)
    result
     (if (= (first alphabet) (first word))
      (recur (rest alphabet) word result)
      (recur (rest alphabet) word (cons (cons (first alphabet) word) result))
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
  (if (= n 0)
    wordsSequence
    (recur alphabet (generateWords alphabet wordsSequence '()) (- n 1))
  )
)

(defn wordGeneratorFactory [n]
  (fn [alphabet] (generate alphabet, '(()), n)))

(let [wordGenerator3 (wordGeneratorFactory 3)]
  (println (wordGenerator3 '(a b c d))))