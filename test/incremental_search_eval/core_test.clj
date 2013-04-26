(ns incremental-search-eval.core-test
  (:require [clojure.math.numeric-tower :as math])
  (:use clojure.test
        incremental-search-eval.core))

(deftest average-precision-test
  (testing "Ability to determine average precision"
    (let [alg1
              {:relevant-ranks [1 5]
               :unjudged-ranks [3]}
          alg2
              {:relevant-ranks [1 7]
               :unjudged-ranks [5 9 10]}]
      (are [x y] (= x y) 

           (/
             (+ 1/1 2/5)
             2)

           (average-precision-from-ranks (:relevant-ranks alg1))

           (/
             (+ 1/1 2/7)
             2)
           (average-precision-from-ranks (:relevant-ranks alg2))

           (/ 
             (+ 1/2 2/4 3/5 4/7)
             4)
           (average-precision-from-ranks [2 4 5 7])

           (/ 
             (+ 1/1 2/2 3/3 4/4 5/5)
             5)
           (average-precision-from-ranks [1 2 3 4 5])))))

(deftest document-weights-test
  (testing "Ability to correctly determine document weights")
    (are [x y] (= x y)

  
         ;; Imagine two algorithms with rankings like
         ;;  
         ;;  Alg A                    Alg B
         ;; 
         ;;  1,5 relevant             1,5,7 relevant 
         ;;  3 is unknown             3 is unknow
         ;;         The rest are irrelevant
         ;;
         ;;
         ;;
         (math/abs
           (- 
             (+ 2/3 1/3 1/5)
             (+ 2/3 1/3 1/5 1/7)))
         (calculate-document-weight-complex 3 3 [1 5] [1 5 7])

                                                                                                         
         ;; Alg A                                  ;; Alg B                                              
         ;; looks something like                   ;; looks something like                                        
         ;; 1 irrelevant                           ;; 1 relevant                                                  
         ;; 2 relevant                             ;; 2 relevant                                                         
         ;; 3 irrelevant                           ;; 3 relevant                                                         
         ;; 4 relevant                             ;; 4 relevant                                                         
         ;; 5 relevant                             ;; 5 relevant                                                          
         ;; 6 unknown (lets call this alpha)       ;; 6 irrelevant                                                        
         ;; 7 relevant                             ;; 7 irrelevant                                                        
         ;;                                        ;; 8 irrelevant                                                        
         ;;                                        ;; 9 unknown (lets call this alpha)                                    
         
         (math/abs                                                                                                    
           (-                                                                                                     
             (+ 4/6 1/6 1/7)                                                                                    
             (+ 6/9 1/9)))                                                                               
                                                                                                         
         (calculate-document-weight-complex 6 9 [2 4 5 7] [1 2 3 4 5])                                   
                                                                                                    
                                                                                                         
         ;; Alg A                                  ;; Alg B                                              
         ;; looks something like                   ;; looks something like                                        
         ;; 1 unknown (lets call this alpha)       ;; 1 irrelevant                                                  
         ;; 2 unknown                              ;; 2 relevant                                                         
         ;; 3 relevant                             ;; 3 unknown (lets call this alpha)                                                         
         ;; 4 irrelevant                           ;; 4 relevant                                                         
         ;; 5 relevant                             ;; 5 irrelevant                                                          
         ;; 6 relevant                             ;; 6 irrelevant                                                        
         ;; 7 relevant                             ;; 7 irrelevant                                                        
         ;;                                        ;; 8 relevant                                                        
         ;;                                        ;; 9 unknown 
         ;;                                        
         ;; And we are trying to find the weight of alpha
                                                                                             
         (math/abs
           (- 
             (+ 1/1 1/1 1/3 1/5 1/6 1/7)
             (+ 2/3 1/3 1/4 1/8)))
         (calculate-document-weight-complex 1 3 [3 5 6 7] [2 4 8])))

                                                                                     
