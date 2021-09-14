#lang racket

(provide encode decode)

(define (encode m)
  (reduce-fun "enc" (string->list (normalize m)) 0 (list)))

(define (decode m) 
  (reduce-fun "dec" (string->list (normalize m)) 0 (list)))

(define (reduce-fun t m qtd acc)
  (define calc
    (if (equal? t "enc")
        calculate-encode
        calculate-decode))

  (if (empty? m) 
      (list->string acc)

      (let ([f (first m)] [r (rest m)])
        (if (and (equal? qtd 5) (equal? t "enc"))
            (reduce-fun t m 0 (append acc (list #\space)))

            (reduce-fun 
             t
             r
             (add1 qtd) 
             (append acc (list (calc f))))))))

(define (calculate-decode c)
  (if (char-numeric? c) c
      (integer->char (+ 97 (- 122 (char->integer c))))))

(define (calculate-encode c)
  (if (char-numeric? c) c
      (integer->char (- 122 (- (char->integer c) 97)))))

(define (normalize s)
  (string-downcase (string-replace s #px"\\s|\\.|\\," "")))