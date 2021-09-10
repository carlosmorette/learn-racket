#lang racket

(provide 
  sum-of-squares 
  square-of-sum 
  difference
)

(define (square-of-sum n) 
  (expt 
    (for/sum ([x (in-range 0 (+ n 1))]) x) 
    2))

(define (sum-of-squares n) 
  (for/sum ([x (in-range 0 (+ n 1))]) (expt x 2)))

(define (difference n) 
 (- (square-of-sum n) (sum-of-squares n)))

