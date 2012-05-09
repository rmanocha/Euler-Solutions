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
              (> counter nn) true
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

(defn problem-4
  ([] (problem-4 999 999))
  ([x y]
   (loop [xx x yy y max-pal 101]
     (if (< xx 100)
       max-pal
       (let [new-xx (if (< yy 100) (dec xx) xx) new-yy (if (< yy 100) 999 (dec yy))]
         (let [xy (* xx yy)]
           (if (> xy max-pal)
             (if (is-palindrome (Integer/toString xy))
               (recur new-xx new-yy xy)
               (recur new-xx new-yy max-pal))
             (recur new-xx new-yy max-pal))))))))

(defn problem-5 []
  (loop [prod 2520]
    (if (every? zero? (map #(mod prod %) '(20 19 18 17 16 15 14 13 12 11)))
      prod
      (recur (+ 2520 prod)))))

(defn square [n] (* n n))

(defn problem-6 [n]
  (- (square (reduce + (range 1 (+ n 1)))) (reduce + (map square (range 1 (+ n 1))))))

(defn gen-all-primes
  ([] (cons 2 (gen-all-primes 3)))
  ([n]
   (lazy-seq
     (if (is-prime n)
       (cons n (gen-all-primes (+ 2 n)))
       (gen-all-primes (+ 2 n))))))

(defn problem-7 [n]
  (last (take n (gen-all-primes))))

(def num-str-8 "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")

(defn problem-8
  ([] (problem-8 0))
  ([n]
   (let [len (.length num-str-8)]
     (loop [nn n max-prod 0]
       (if (= nn (- len 5))
         max-prod
         (let [prod (reduce * (map #(Integer/parseInt (.toString %)) (.substring num-str-8 nn (+ 5 nn))))]
           (recur (inc nn) (if (> prod max-prod) prod max-prod))))))))

(defn problem-9 []
  (reduce *
    (flatten
      (take 1
       (for [a (range 5 1000 5) b (range a 1000 5) c (range b 1000 5) :when (and (= (+ a b c) 1000) (= (+ (square a) (square b)) (square c)))]
        [a b c])))))

(defn problem-10 []
  (reduce + (gen-primes 2000000)))

(defn -main [& args]
  (time (problem-1 1000))
  (println (problem-1 1000))
  (time  (problem-2 4e6))
  (println  (problem-2 4e6))
  (time (problem-3 600851475143))
  (println (problem-3 600851475143))
  (time (problem-4))
  (println (problem-4))
  (time (problem-5))
  (println (problem-5))
  (time (problem-6 100))
  (println (problem-6 100))
  (time (problem-7 10001))
  (println (problem-7 10001))
  (time (problem-8))
  (println (problem-8))
  (time (problem-9))
  (println (problem-9))
  (time (problem-10))
  (println (problem-10))
  )
