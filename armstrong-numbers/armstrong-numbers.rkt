#lang racket

(provide armstrong-number?)

(define (armstrong-number? n)
  (define list-numbers (remove* (list "") (string-split (number->string n) "")))
  (if (equal? n (do-check (length list-numbers) list-numbers 0)) #t #f))

(define (do-check len digits acc)
  (if (empty? digits) acc
    (do-check len (rest digits) (+ acc (expt (string->number (first digits)) len)))))
