#lang racket

(provide etl)

(define (etl input)
  (make-hash (sort-result 
    (for/fold ([acc (list)]) ([k (hash-keys input)]) 
      (if (< k 0) (exn:fail:contract "invalid key")
        (append acc (build k (hash-ref input k) (list))))))))

(define (sort-result list-of-pairs)
  (sort list-of-pairs #:key car string<?))

(define (build key letters acc)
  (if (empty? letters) 
    acc 
    (let ([first-character (first letters)])
      (if (not (regexp-match #px"[a-zA-Z]" first-character)) (error "invalid character")  
        (build key 
               (rest letters) 
               (append acc (list (cons (string-downcase first-character) key))))))))
