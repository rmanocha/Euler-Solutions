(ns euler-solutions.core)

(defn problem-1 []
  (let [mul-sum 0]
    (for [index (range 1000)]
      (cond
        (= 0 (mod index 3)) (+ mul-sum index)
        (= 0 (mod index 5)) (+ mul-sum index)))
    mul-sum))

(defn -main [& args]
  (println (problem-1)))
