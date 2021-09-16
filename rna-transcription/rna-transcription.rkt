#lang racket

(provide to-rna)

(define (to-rna rna)
  (if (empty? rna) ""
    (reduce-fun (string->list rna) "")))

(define (reduce-fun rna-lst acc)
  (if (empty? rna-lst) 
    acc
    (reduce-fun 
      (rest rna-lst) 
      (string-append acc 
                     (match (first rna-lst)
                       [#\C "G"]
                       [#\G "C"]
                       [#\T "A"]
                       [#\A "U"])))))
