(defn positive? [value]
  (Thread/sleep 50)
  (< 0 value))

(defn splittingIntoParts [threadIndex, threadsNum, coll]
  (take (quot (count coll) threadsNum)
    (drop (* threadIndex (quot (count coll) threadsNum)) coll)))


(defn makeParts [coll threadsNum]
  (map (fn [threadIndex] (splittingIntoParts threadIndex threadsNum coll)) (range 0 threadsNum)))

(defn myFilter[predicate, coll, threadsNum]
  (flatten (map deref (doall
    (map (fn [collPart] (future (doall (filter predicate collPart)))) (makeParts coll threadsNum))))))

(let [processingSequences (fn [coll, threadNum] (myFilter positive? coll threadNum))]
(println "======================START======================")
(time (doall (filter positive? (range 0 200))))
(time (processingSequences (range 0 200) 1))
(time (processingSequences (range 0 200) 2))
(time (processingSequences (range 0 200) 5))
(time (processingSequences (range 0 200) 7))
(println "====================THE END====================="))