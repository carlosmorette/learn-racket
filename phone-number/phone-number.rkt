#lang racket

(provide nanp-clean clean-str exception?)

(define (nanp-clean s) 
  (define safe-str (clean-str s))
  (if (or (invalid-characters? safe-str) (not (valid-length? safe-str)))
    (error "invalid phone number")
    (rule (string-replace safe-str #rx"\\+" ""))
    ))

(define (rule str)
  (if (= (string-length str) 11)
    (if (not (equal? (string-ref str 0) #\1))
      (error "invalid phone number")
      (ten-length (list->string (rest (string->list str))))) 
    (ten-length str)))

(define (ten-length str)
  (if (or 
        (equal? (string-ref str 0) #\1)
        (equal? (string-ref str 0) #\0)
        (equal? (string-ref str 3) #\1)
        (equal? (string-ref str 3) #\0)
        )
    (error "invalid phone number")
    str
    ))

(define (clean-str str)
  (string-replace str #px"\\(|\\)|-|\\s|\\." ""))

(define (valid-length? str)
  (if (< (string-length str) 10)
    (error "invalid phone number")
    (if (or (exception? str) (equal? (string-length str) 10))
      #t
      (error "invalid phone number")
      )))

(define (invalid-characters? str)
  (regexp-match #rx"[a-z]|@|:|!" str))

(define (exception? str)
  (if (or 
        (and 
          (equal? (string-length str) 12) 
          (equal? (string-ref str 0) #\+)
          (equal? (string-ref str 1) #\1))
        (and
          (equal? (string-length str) 11)
          (equal? (string-ref str 0) #\1)))
    #t
    #f
    ))

