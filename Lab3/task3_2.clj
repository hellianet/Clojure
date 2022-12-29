(defn positive? [value]
  (Thread/sleep 50)
  (< 0 value))

(defn splittingIntoParts [threadIndex, pieceSize, coll]
  (take pieceSize (drop (* threadIndex pieceSize) coll)))

(defn makeParts [coll threadsNum]
  (map (fn [threadIndex] (splittingIntoParts threadIndex (quot (count coll) threadsNum) coll)) (range 0 threadsNum)))

(defn sliceInfinity [coll pieceSize threadNum]
   (if (empty? coll)
     ()
     (lazy-seq (cons (take (* pieceSize threadNum) coll)
       (sliceInfinity (drop (* pieceSize threadNum) coll) pieceSize threadNum)))))

(defn myFilter[predicate, coll, threadsNum, pieceSize]
   (mapcat (fn [infinityPart] (map deref (doall
    (map (fn [collPart] (future (doall (filter predicate collPart)))) (makeParts infinityPart threadsNum)))))
      (sliceInfinity coll pieceSize threadsNum)))

(let [processingSequences (fn [coll, threadNum, pieceSize] (myFilter positive? coll threadNum pieceSize))]
(println "======================START======================")
(time (doall (filter positive? (range 0 200))))
(time (doall (processingSequences (range 0 200) 1 200)))
(time (doall (processingSequences (range 0 200) 2 200)))
(time (doall (processingSequences (range 0 200) 5 200)))
(time (doall (processingSequences (range 0 200) 7 200)))
(println "====================THE END====================="))