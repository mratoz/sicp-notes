#lang racket
(require "base.rkt")
(require racket/trace)

(define sum-int
  (lambda (a b)
    (cond
      ((> a b) 0)
      (else (+ a (sum-int (+ 1 a) b))))))

(define sum-square
  (lambda (a b)
    (cond
      ((> a b) 0)
      (else (+ (square a) (sum-square (+ 1 a) b))))))