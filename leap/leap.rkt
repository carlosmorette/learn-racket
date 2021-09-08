#lang racket

(provide leap-year?)

(define (leap-year? year)
  (if (equal? (modulo year 4) 0)
    (if (equal? (modulo year 100) 0)
      (if (equal? (modulo year 400) 0)
        #t
        #f)
      #t)
    #f)
  )
