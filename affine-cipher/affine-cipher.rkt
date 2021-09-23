#lang racket

(provide (contract-out
          [encode (string?
                   exact-nonnegative-integer?
                   exact-nonnegative-integer? . -> . string?)]
          [decode (string?
                   exact-nonnegative-integer?
                   exact-nonnegative-integer? . -> . string?)]))

(define alphabet (range 97 123))
(define alphabet-length (length alphabet))

(define (encode msg a b)
  (find-inverse a)
  (list->string 
    (encode-reduce-fun a b 0 (string->list (normalize msg)) (list))))

(define (encode-reduce-fun a b qtd list-char acc)
  (if (empty? list-char) 
    acc
    (let* ([fst (first list-char)] 
            [rst (rest list-char)]
            [char-fst (char->integer fst)])

        (cond 
          [(not (member char-fst alphabet)) 
            (encode-reduce-fun a b (add1 qtd) rst (append acc (list fst)))]

          [(equal? qtd 5) 
           (encode-reduce-fun a b 0 list-char (append acc (list #\space)))]

          [else
              (define new-acc 
                (append acc 
                  (list (at (modulo (+ (* a (find-index char-fst)) b) alphabet-length)))))

              (encode-reduce-fun a b (add1 qtd) rst new-acc)
              ]))))


(define (decode msg a b)
  (define inverse-mod (find-inverse a))
  (list->string 
    (decode-reduce-fun inverse-mod b
                       (string->list (normalize msg)) (list))))

(define (decode-reduce-fun inverse-mod b list-char acc)
  (if (empty? list-char)
    acc
    (let* ([fst (first list-char)]
           [rst (rest list-char)]
           [char-fst (char->integer fst)])

      (cond 
        [(not (member char-fst alphabet))
          (decode-reduce-fun inverse-mod b rst (append acc (list fst)))]

        [else
            (define result (at (modulo (* inverse-mod 
                                  (- (find-index char-fst) b)) alphabet-length)))

            (decode-reduce-fun inverse-mod b rst 
                               (append acc (list result)))
          ]))))

(define (find-inverse n [c 1])
  (if (> c alphabet-length) 
    (error "invalid number")
    (if (equal? (modulo (* n c) alphabet-length) 1)
      c
      (find-inverse n (add1 c)))))

(define (normalize txt)
  (string-replace (string-downcase txt) #px"\\s|\\.|\\," ""))

(define (find-index char) 
  (index-of alphabet char))

(define (at index) 
  (integer->char (list-ref alphabet index)))
