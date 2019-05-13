(def num_threads 32)
(System/setProperty "clojure.core.async.pool-size" (str num_threads))

(require '[clojure.edn :as edn])
(require '[clojure.core.async :as async :refer [go <!! <!]])


(defn vecFromFile 
    ; We know the number of items + delimited by newline (for numbers.txt)
    ([path n]
        (with-open [f (clojure.java.io/reader path)]
            (mapv edn/read-string (take n (rest (line-seq f)))) ))
    ; First item in file is number of items + delimited by space
    ([path]
        (let [[n & items] (clojure.string/split (slurp path) #"[ ]")]
            (mapv edn/read-string (take (edn/read-string n) items)) )) )

(defn sort_two [a b] (if (< a b) [a b] [b a]))

; https://stackoverflow.com/a/5980031
(defn swap [v i1 i2] 
    (assoc v i2 (v i1) i1 (v i2)))

(defn _quicksort [v]
    (go (condp >= (count v)
        1 v
        2 (apply sort_two v)
        (loop  [v v  lo 1  hi 1]
            (if (< hi (count v))
                (if (> (v hi) (v 0))
                    (recur v lo (inc hi))
                    (recur (swap v hi lo) (inc lo) (inc hi)) )
                (let [left (_quicksort (subvec v 1 lo))
                    right (_quicksort (subvec v lo hi))]
                    (concat (<! left) [(v 0)] (<! right)) ))))))

(defn quicksort [v] (time (<!! (_quicksort v))))

(defn verify [v]
    (if
        (reduce (fn [a b] (if (<= a b) b (reduced false))) v)
        nil ; (println "Sequence correctly sorted in ascending order")
        (println "Sequence failed to sort correctly!") ))


(println "Running with" num_threads "threads")
; Tried removing tedium of re-running multiple times, but there's some
; kind of caching happening that made the times ridiculously fast
(dotimes [n 1]
    (println "== Run #" n "==")
    (let   [numbers_path "number_dat/100.txt"
            initial_vec (vecFromFile numbers_path)]
        ; (println "Sorting " numbers_path " with " num_threads " threads...")
        (verify (quicksort initial_vec)) )

    (let   [numbers_path "number_dat/10000.txt"
            initial_vec (vecFromFile numbers_path)]
        ; (println "Sorting " numbers_path " with " num_threads " threads...")
        (verify (quicksort initial_vec)) )

    (let   [numbers_path "numbers.txt"
            initial_vec (vecFromFile numbers_path 1000000)]
        ; (println "Sorting " numbers_path " with " num_threads " threads...")
        (verify (quicksort initial_vec)) ) )

