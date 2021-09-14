#lang racket

(provide word-count reduce-fun remove-pontuation normalize-and-split)

(define (word-count str)
  (define lst (normalize-and-split str))
  (reduce-fun lst (hash)))

(define (reduce-fun lst acc)
  (if (equal? (length lst) 0) acc

    (if (hash-has-key? acc (first lst)) 
      (reduce-fun 
        (rest lst) 
        (hash-update acc (first lst) add1))
  
      (reduce-fun 
        (rest lst) 
        (hash-set acc (first lst) 1)))))

(define (normalize-and-split str)
  (remove-pontuation 
    (string-split 
      (string-downcase str) 
      #px"\\s+|;|,")))

(define (remove-pontuation lst)
  (define x 
    (map (lambda (i) 
        (string-replace i #px"[\\s+|;|,|\\.|\n+|!!&@$%^&|:]" "")) 
       lst))

  (define z 
    (map (lambda (i)
           (if (equal? i "can't") i
             (string-replace i #px"[\']" ""))) x))

  (filter 
    (lambda (i) (non-empty-string? i)) z))
