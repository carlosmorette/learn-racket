#lang racket

(provide 
  make-robot
  name
  reset!
  reset-name-cache!
  )

(define ascii-letters (range 65 90))

(define used-names (make-hash))
(define (set-used-name name)
  (hash-set! used-names name #t))

(define (is-member? name)
  (hash-has-key? used-names name))

(define (random-letters)
  (integer->char (list-ref ascii-letters (random 0 25))))

(define (random-numbers)
  (random 99 1000))

(define (random-robot-name)
  (string-append 
    (list->string 
      (list (random-letters) (random-letters))) 
    (~a (random-numbers))))

(define (check-name name)
  (if (is-member? name)
    (check-name (random-robot-name))
    name))

(define (make-robot)
  (define name (check-name (random-robot-name)))
  (make-hash (list 
               (cons "name" name))))

(define (name robot)
  (hash-ref robot "name"))

(define (reset! robot)
  (hash-set! robot "name" (random-robot-name)))

(define max-names (* 26 26 10 10 10))

(define (reset-name-cache!) 
  (hash-clear! used-names))

