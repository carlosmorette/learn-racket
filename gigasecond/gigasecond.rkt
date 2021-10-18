#lang racket

(provide add-gigasecond)

(define billion 1000000000)

(require racket/date)


(define (add-gigasecond date)
  (seconds->date (+ billion (date->seconds date))))
