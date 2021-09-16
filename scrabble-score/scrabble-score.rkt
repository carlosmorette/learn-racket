#lang racket

(provide score do-match)

(define groups
  (list 
    (cons 1 (list "a" "e" "i" "o" "u" "l" "n" "r" "s" "t"))
    (cons 2 (list "d" "g"))
    (cons 3 (list "b" "c" "m" "p"))
    (cons 4 (list "f" "h" "v" "w" "y"))
    (cons 5 (list "k"))
    (cons 8 (list "j" "x"))
    (cons 10 (list "q" "z"))))

(define (score str) 
  (accumulate-fun (string->list (string-downcase str)) 0))

(define (accumulate-fun lst acc)
  (if (empty? lst) acc
    (accumulate-fun (rest lst) (+ acc (do-match (first lst))))
  ))

(define (do-match letter) 
  (car 
    (findf 
      (lambda (i) (member (~a letter) (cdr i))) groups)))
