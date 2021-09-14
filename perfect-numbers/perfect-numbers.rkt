#lang racket

(provide classify accumulate)

(define (accumulate n)
  (for/sum ([i (in-range 1 n)]) 
    (if (equal? (modulo n i) 0)
        i
        0)))

(define (classify n)
  (define result (accumulate n))
  (cond 
    [(equal? result n) 'perfect]
    [(> result n) 'abundant]
    [(< result n) 'deficient]))

