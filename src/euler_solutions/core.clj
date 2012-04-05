(ns euler-solutions.core
  (:require clojure.contrib.string))

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

; #3
(defn is-prime [n]
  (cond
    (< n 2) false
    (= n 2) true
    (even? n) false
    :else (loop [counter 2 nn (Math/sqrt n)]
            (cond
              (>= counter nn) true
              (= 0 (mod n counter)) false
              :else (recur (inc counter) nn)))))

(defn gen-primes [n]
  (cond
    (< n 3) '(2)
    (even? n) (gen-primes (dec n))
    :else
      (lazy-seq
        (if (is-prime n)
          (cons n (gen-primes (- n 2)))
          (gen-primes (- n 2))))))

(defn problem-3 [n]
  (first (filter #(= 0 (mod n %)) (take-while (partial < 1) (gen-primes (int (Math/sqrt n)))))))

(defn is-palindrome [s]
  (= s (clojure.contrib.string/reverse s)))

(defn problem-4 []
  (loop [x 999 y 999]
    (let [xy (* x y)]
      (cond
        (is-palindrome (Integer/toString xy)) xy
        (< x 101) false
        (< y 101) (recur (dec x) 999)
        :else (recur x (dec y))))))


(defn -main [& args]
  (println (problem-1 1000))
  (println (problem-2 4e6))
  (println (problem-3 600851475143)))
