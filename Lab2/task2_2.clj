(defn simpleShift [x] (+ x 5))
(defn sqr [x] (* x x))

(defn area [f, start, end]
  (* (- end start) (* 0.5 (+ (f start) (f end)))))

(defn areaWithDelta [f, start, end, delta]
  (* (* (- end start) delta)
     (* 0.5 (+ (f (* delta start)) (f (* delta end))))))

(defn valuesForIntegrate [f, delta]
  (reductions + (map (fn [x] (areaWithDelta f x (+ x delta) delta))
    (iterate (fn [x] (+ x delta)) 0))))

(defn integrateTo [f, end, delta, history]
  (let [s (int (/ end delta))]
    (+ (area f (- end (- end (* delta s))) end)
     (nth history s))))

(defn seqIntegrate [f]
  (let [delta 1, history (valuesForIntegrate f delta)]
    (fn [x] (integrateTo f x delta history))))

(let [simpleShiftIntregral (seqIntegrate simpleShift), sqrIntegral (seqIntegrate sqr)]
(println "======================START======================")
(time (simpleShiftIntregral 1000))
(time (simpleShiftIntregral 900))
(println "=================================================")
(time (sqrIntegral 1000))
(time (sqrIntegral 900))
(println "====================THE END====================="))