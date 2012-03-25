(ns euler-solutions.core)

(defn problem-1 [n]
  (loop [nn 0 mul-sum 0]
    (if (= n nn)
      mul-sum
      (cond
        (= 0 (mod nn 3)) (recur (inc nn) (+ nn mul-sum))
        (= 0 (mod nn 5)) (recur (inc nn) (+ nn mul-sum))
        :else (recur (inc nn) mul-sum)))))

; This comes from http://icyrock.com/blog/2011/07/solving-project-euler-problem-2-in-clojure/
(defn fibs
  ([] (fibs 1 2))
  ([a b] (lazy-seq (cons a (fibs b (+ a b))))))

(defn problem-2 [n]
  (reduce + (filter #(= 0 (mod % 2)) (take-while (partial > n) (fibs)))))

(defn -main [& args]
  (println (problem-1 1000))
  (println (problem-2 4e6)))
