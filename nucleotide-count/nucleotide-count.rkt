#lang racket

(provide nucleotide-counts)

;(define (nucleotide-counts str)
;  (reduce-fun (string->list str) (hash #\A 0 #\C 0 #\G 0 #\T 0)))

;(define (reduce-fun lst acc)
;  (if (empty? lst) (hash->list acc)
;    (reduce-fun (rest lst) (hash-update acc (first lst) add1))))

(define nucleotides (list #\A #\C #\G #\T))

(define (nucleotide-counts str)
  (let ([lst (string->list str)])
  
    (for/list ([i nucleotides])
      (cons i (length (filter 
                      (lambda (x) 
                        (and (nucleotide? x) (equal? x i))) lst))))))

(define (nucleotide? n)
  (if (member n nucleotides) #t (error "invalid nucleotide")))
