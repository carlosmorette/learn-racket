#lang racket

(provide isogram?)

(define (isogram? s)
  (define filtered
    (filter (lambda (x) 
              (regexp-match #rx"[a-zA-Z]" (string x))) (string->list s)))

  (define convertion 
    (map (lambda (x) 
           (string-downcase (string x))) filtered))

  (reducer convertion (list)))

(define (reducer los state)
  (if (empty? los) #t
      (if (member (first los) state) #f 
          (reducer 
           (rest los) 
           (append (list (first los)) state)))))
