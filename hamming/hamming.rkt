#lang racket

(provide hamming-distance)

(define (hamming-distance str1 str2)
  (if (and (empty? str1) (empty? str2)) 0
    (if (not (= (string-length str1) (string-length str2))) (error)

  (for/fold ([acc 0])
    ([x (string->list str1)] 
     [y (string->list str2)])
    (if (equal? x y) acc (add1 acc)))                                           
  )))
