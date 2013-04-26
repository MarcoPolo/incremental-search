(ns incremental-search-eval.core
  (:require [clojure.math.numeric-tower :as math]))

(defn mean [numbers]
  (/ (reduce + numbers) (count numbers)))

(defn mean-average-precision 
  "Calculate the mean of the AP of a set of topics"
  [average-precisions]
  (mean average-precisions))

(defn average-precision 
  "Calculate the average precision for a topic
   given the relevant doc precisions  e.g. [ 1/1 2/3 3/5 ]"
  [document-precisions]
  (mean document-precisions))


(defn calculate-document-weight-simple 
  "A heavier weighted document is a document that provides more insight
   on the difference of two algorithms.
   The rank specifies where the return ranked in the results from an algorithm"
  [rank-alg1 rank-alg2]
  (math/abs (- (/ 1 rank-alg1) (/ 1 rank-alg2))))

(defn precision-of-rank 
  "Give a rank and the list of relevant docs to get the precision"
  [relevant-ranks rank] 
  (/ (inc (.indexOf relevant-ranks rank)) rank))

(defn average-precision-from-ranks [relevant-ranks]
  (average-precision (map (partial precision-of-rank relevant-ranks) relevant-ranks)))

(defn effect-on-ap [relevant-ranks rank-of-unknown-doc ]
  (let [relevant-docs-before-unknown (take-while (partial > rank-of-unknown-doc) relevant-ranks) 
        relevant-docs-after-unknown (drop-while (partial > rank-of-unknown-doc) relevant-ranks)]
    (+ 
      (/ (inc (count relevant-docs-before-unknown)) rank-of-unknown-doc)
      (/ 1 rank-of-unknown-doc)
      (reduce + (map #(/ 1 %) relevant-docs-after-unknown)))))

(defn splice-in-relevant-rank [relevant-ranks rank-of-new-relvant]
  (concat 
    (take-while (partial > rank-of-new-relvant) relevant-ranks) 
    [rank-of-new-relvant] 
    (drop-while (partial > rank-of-new-relvant) relevant-ranks)))

(defn calculate-document-weight-complex 
  "A better, yet more complex way to calculate the weight of document
   based of magic"
  [rank-of-unknown-doc-alg1 rank-of-unknown-doc-alg2 relevant-ranks-alg1 relevant-ranks-alg2]
  (math/abs 
    (- 
      (effect-on-ap relevant-ranks-alg1 rank-of-unknown-doc-alg1 )
      (effect-on-ap relevant-ranks-alg2 rank-of-unknown-doc-alg2 ))))
  

(defn how-many-docs-catch-up
  "Calculate the number of topics for the other guy to catch up, best case scenario"
  [enemy-AP doc-unjudged-ranks relevant-ranks]
  (let [average-precision (average-precision-from-ranks relevant-ranks)
        delta-AP (- enemy-AP average-precision)]

    ;; We want to calculate the unjudged-ranks until we get a value > delta-AP
    (loop [current-AP-diff 0 docs-needed 0 relevant-ranks relevant-ranks doc-unjudged-ranks doc-unjudged-ranks]
      (cond
        (> current-AP-diff delta-AP) docs-needed
        (empty? doc-unjudged-ranks) -1 ;; Impossible to catch up
        :else
        (recur
          (+ current-AP-diff (effect-on-ap relevant-ranks (first doc-unjudged-ranks)))
          (inc docs-needed)
          (splice-in-relevant-rank relevant-ranks (first doc-unjudged-ranks))
          (rest doc-unjudged-ranks))))))

(def cut-off-point-lower-bound 10)

(defn done-with-topic? 
  "Calculates to check if we are done with a topic"
  [[relevant-ranks-alg1 unjudged-ranks-alg1] [relevant-ranks-alg2 unjudged-ranks-alg2]]
  (let [greater (max-key average-precision-from-ranks [relevant-ranks-alg1 relevant-ranks-alg2])
        lesser (min-key average-precision-from-ranks [relevant-ranks-alg1 relevant-ranks-alg2])]
    (if (= relevant-ranks-alg1 greater)
      (how-many-docs-catch-up (average-precision-from-ranks greater) unjudged-ranks-alg2 relevant-ranks-alg2) 
      (how-many-docs-catch-up (average-precision-from-ranks greater) unjudged-ranks-alg1 relevant-ranks-alg1))))


(comment
  "DEMO"

  (def alg1
    {:relevant-ranks [1 5]
     :unjudged-ranks [3]})

  (def alg2
    {:relevant-ranks [1 7]
     :unjudged-ranks [5 9 10]})

  (/
    (+ 1/1 2/5)
    2)
  (empty? '())

  (average-precision-from-ranks (:relevant-ranks alg1))

  (how-many-docs-catch-up 7/10 [10 120] [7 9])

  (effect-on-ap [7 9 10] 120)
  (splice-in-relevant-rank [7 9 10] 120)


  (double (+ 2/3 1/5))
  (effect-on-ap (:relevant-ranks alg1) 3)

  (calculate-document-weight-complex 3 3 [1 5] [1 5 7])

  ;; Example from paper
  (average-precision-from-ranks [1 5])
  (average-precision-from-ranks [1 3 5])

  (effect-on-ap [1 5] 3)

  ;; Using this as a calculator
  (math/abs
    (-
      6/5
      (+ 1/5 1/7 1)))

  (math/abs
    (- 
      (+ 2/3 1/3 1/5)
      (+ 2/3 1/3 1/5 1/7)))

  ;; Alg A
  ;; looks something like 
  ;; 1 irrelevant
  ;; 2 relevant
  ;; 3 irrelevant
  ;; 4 relevant
  ;; 5 relevant
  ;; 6 unknown (lets call this alpha)
  ;; 7 relevant
  ;; 
  ;; Calculate the AP
  (/ 
    (+ 1/2 2/4 3/5 4/7)
    4)

  ;; Alg B
  ;; looks something like 
  ;; 1 relevant
  ;; 2 relevant
  ;; 3 relevant
  ;; 4 relevant
  ;; 5 relevant
  ;; 6 irrelevant
  ;; 7 irrelevant
  ;; 8 irrelevant
  ;; 9 unknown (lets call this alpha)
  ;;
  ;; Calculate the AP
  (/ 
    (+ 1/1 2/2 3/3 4/4 5/5)
    5)


  ;; Calculate the document weight of some doc alpha appearing at rank 6 in alg A and rank 9 in alg B
  (- 
    (+ 4/6 1/6 1/7)
    (+ 6/9 1/9))

  ;; 25/126

  (calculate-document-weight-complex 6 9 [2 4 5 7] [1 2 3 4 5])

  ;; 25/126 ... Nice!


  )

