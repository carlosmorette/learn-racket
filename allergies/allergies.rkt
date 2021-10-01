#lang racket

(provide list-allergies allergic-to?)

(define allergies '("eggs"
                    "peanuts"
                    "shellfish"
                    "strawberries"
                    "tomatoes"
                    "chocolate"
                    "pollen"
                    "cats"))
         

(define (list-allergies score)
  (define (rec score allergies)
    (cond
      [(or (= 0 score) (= 0 (length allergies))) '()]
      [(= 1 (modulo score 2))
       (cons (car allergies) (rec (quotient score 2) (cdr allergies)))]
      [else (rec (quotient score 2) (cdr allergies))]))
  (rec score allergies))

(define (allergic-to? str x)
  (not (not (member str (list-allergies x)))))
