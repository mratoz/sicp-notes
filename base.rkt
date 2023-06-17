#lang racket
(provide square average)

(define square
  (lambda (x)
    (* x x)))
(define average
  (lambda (x y)
    (/ (+ x y) 2)))