(defn generateWord [alphabet, word]
  (if (empty? alphabet)
    '()
     (if (.startsWith word (first alphabet))
       (generateWord (rest alphabet) word)
       (cons (str (first alphabet) word) (generateWord (rest alphabet) word))
    )
  )
)

(defn generateWords [alphabet, wordsSequence]
  (if (empty? wordsSequence)
    '()
    (concat (generateWord alphabet (first wordsSequence)) (generateWords alphabet (rest wordsSequence)))
  )
)

(defn generate [alphabet, wordsSequence, n]
  (if (= n 1)
    wordsSequence
    (generate alphabet (generateWords alphabet wordsSequence) (- n 1))
  )
)

(defn wordGeneratorFactory [n]
  (fn [alphabet] (generate alphabet, alphabet, n)))

(let [wordGenerator3 (wordGeneratorFactory 3)]
  (println (wordGenerator3 '("a" "b" "c"))))