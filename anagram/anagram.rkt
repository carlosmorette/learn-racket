#lang racket

(provide anagrams-for)

(define (anagrams-for txt lst)
  (define t (sort-item txt))
  (for/fold ([acc (list)]) ([item lst]) 
    (if (and 
          (equal? t (sort-item item)) 
          (not (equal? txt item)))
      (append acc (list item))
      acc)))

(define (sort-item item)
  (sort (string->list (string-downcase item)) char<?))
