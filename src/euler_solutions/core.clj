(ns euler-solutions.core)

(defn problem-1 [n]
  (loop [nn 0 mul-sum 0]
    (if (= n nn)
      mul-sum
      (cond
        (= 0 (mod nn 3)) (recur (inc nn) (+ nn mul-sum))
        (= 0 (mod nn 5)) (recur (inc nn) (+ nn mul-sum))
        :else (recur (inc nn) mul-sum)))))

(defn -main [& args]
  (println (problem-1 1000)))
