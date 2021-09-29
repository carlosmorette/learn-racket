#lang racket

(provide acronym)

(define (acronym str)
  (define safe-str 
    (string-split (string-replace str #px"_" "") #px"\\s|\\-"))

  (define result 
    (for/fold ([acc ""]) ([word safe-str]) 
    (if (non-empty-string? word) 
      (string-append acc (~a (first (string->list word))))
      acc)))

  (string-upcase result))
