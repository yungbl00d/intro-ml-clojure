(ns test.core
  (:gen-class))

(require '[clojure.math :as math])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn dist [probs] (let [sample (rand)
                         cum (reduce (fn [acc item]
                               (conj acc (+ item 
                                            (acc last)
                                            ))) [0] probs)
                         ]
  (prn sample cum)
  (dec (first (first (filter (fn [[i p]] (> p sample))
                 (map-indexed (fn [i p] [i p]) cum))
                     )))
  )
  )

(dist [0.4 0.4 0.2])

(> 1 0)


(def data [
    [0.14, 0.14, 0.28, 0.44],
    [0.22, 0.1,  0.45, 0.33],
    [0.1,  0.19, 0.25, 0.4 ],
    [0.02, 0.08, 0.43, 0.45],
    [0.16, 0.08, 0.35, 0.3 ],
    [0.14, 0.17, 0.31, 0.38],
    [0.05, 0.14, 0.35, 0.5 ],
    [0.1,  0.21, 0.28, 0.44],
    [0.04, 0.08, 0.35, 0.47],
    [0.11, 0.13, 0.28, 0.45],
    [0.0,  0.07, 0.34, 0.65],
    [0.2,  0.05, 0.4,  0.37],
    [0.12, 0.15, 0.33, 0.45],
    [0.25, 0.1,  0.3,  0.35],
    [0.0,  0.1,  0.4,  0.5 ],
    [0.15, 0.2,  0.3,  0.37],
    [0.0,  0.13, 0.4,  0.49],
    [0.22, 0.07, 0.4,  0.38],
    [0.2,  0.18, 0.3,  0.4 ]
])



(defn euclidean [p1 p2] ( ->> (map vector p1 p2)
                              (map (fn [[x1 x2]] (math/pow (- x1 x2) 2)))
                              (reduce +)
                              (math/sqrt)
                         ))

(defn center [clust] (mapv #(/ (reduce + %1) (count %1)) (apply mapv vector clust)))

(defn new-clusters [new-centers] 
  (reduce (fn [acc cur] 
            (let [fkey (first (first (sort-by #(euclidean cur (second %)) new-centers)))
                  ]
              (update acc fkey conj cur)
              )) {} data)
  )

(defn kmeans 
  [data k]
  (let [partitioned 
          (reduce #(update %1 (first %2) conj (second %2)) {} (map vector (cycle (range k)) data))
        ]
    (loop [clusters partitioned]
      (let [centers (update-vals clusters center)
            new (new-clusters centers)]
        (prn centers new)
        (if (= clusters new) clusters 
          (recur new))
        )
      )
    )
)
(kmeans data 4)



;; (def cluster1 (first (vals 
;;                        (group-by first (map vector (cycle (range 3)) data)))))
;;
;; (apply hash-map (interleave (cycle (range 3)) data))

(def ptoned (reduce #(update %1 (first %2) conj (second %2)) {} (map vector (cycle (range 3)) data)))
(def cluster-1 (first (vals ptoned)))



(def centers (update-vals ptoned center))


(def d1 [0.14, 0.14, 0.28, 0.44])
(def d2 [0.22, 0.1, 0.45, 0.33])

(sort-by #(euclidean d2 (second %)) centers)

(reduce (fn [acc cur] 
          (let [fkey (first (first (sort-by #(euclidean cur (second %)) centers)))
                ]
            (update acc fkey conj cur)
            )) {} data)


