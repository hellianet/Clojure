(defn simpleShift [x] (+ x 5))
(defn sqr [x] (* x x))

(defn area [f, start, end]
  (* (- end start) (* 0.5 (+ (f start) (f end)))))

(defn areaWithDelta [f, start, end, delta]
  (* (* (- end start) delta)
     (* 0.5 (+ (f (* delta start)) (f (* delta end))))))

(defn integralSteps [f, steps, delta]
  (loop [n steps, res 0]
    (if (= n 0) 0
      (if (= n 1)
        (+ res (areaWithDelta f 0 1 delta))
        (recur (- n 1) (+ res (areaWithDelta f (- n 1) n delta)))))))

(defn integrateTo [f, stepsFunction, end, delta]
  (let [s (int (/ end delta))]
   (+ (area f (- end (- end (* delta s))) end) (stepsFunction f s delta))))

(defn integrate [f]
   (fn [x]
    (integrateTo f integralSteps x 1)))

(defn memIntegralSteps []
  (let [recurMemIntagrate (fn [recurFunc, res, f, steps, delta]
   (if (= steps 1)
     (+ res (areaWithDelta f 0 1 delta))
      (recurFunc recurFunc (+ res (areaWithDelta f (- steps 1) steps delta)) f (- steps 1) delta)))]
      (partial recurMemIntagrate (memoize recurMemIntagrate) 0)
    ))

(defn memIntegrate [f]
  (fn [x]
    (integrateTo f (memIntegralSteps) x 1)))

(let [simpleShiftIntregral (integrate simpleShift), sqrIntegral (integrate sqr),
      simpleShiftIntregralMem (memIntegrate simpleShift), sqrIntegralMem (memIntegrate sqr)]
(println "======================START======================")
(time (simpleShiftIntregral 150))
(time (simpleShiftIntregral 150))
(println "=================================================")
(time (simpleShiftIntregralMem 150))
(time (simpleShiftIntregralMem 150))
(println "=================================================")
(time (sqrIntegral 100))
(time (sqrIntegral 100))
(println "=================================================")
(time (sqrIntegralMem 100))
(time (sqrIntegralMem 100))
(println "====================THE END====================="))