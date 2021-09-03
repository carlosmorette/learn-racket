#lang racket

(provide square total)

(define (square x) (expt 2 (sub1 x)))

(define (total [x 65]) (sub1 (square x)))
