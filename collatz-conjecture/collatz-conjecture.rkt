#lang racket

(provide collatz)

(define (collatz n)
  (if (<= n 0) 
    (error "zero or negative value is an error")
    (accumulate n 0)))

(define (accumulate n t)
  (if (equal? n 1) t
    (if (equal? (modulo n 2) 0)
      (accumulate (/ n 2) (add1 t))
      (accumulate (add1 (* n 3)) (add1 t)))))
