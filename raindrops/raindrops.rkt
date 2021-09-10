#lang racket

(provide convert calc reduce-fun)

(define (convert number)
    (define result (reduce-fun 3 "" number))
    (if (equal? result "") (~v number) result))

(define (reduce-fun n acc number)
  (if (> n 7) acc
    (if (equal? (modulo number n) 0) 
      (reduce-fun (+ n 2) (calc n acc) number) 
      (reduce-fun (+ n 2) acc number))))

(define (calc n str)
  (case n
    [(3) "Pling"]
    [(5) (string-append str "Plang")]
    [(7) (string-append str "Plong")]))
