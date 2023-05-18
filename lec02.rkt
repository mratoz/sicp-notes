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

(define pi-sum
  (lambda (a b)
    (cond
      ((> a b) 0)
      (else (+ (/ 1 (* a (+ a 2))) (pi-sum (+ a 4) b))))))

(define sum-general
  (lambda (term a next b)
    (cond
      ((> a b) 0)
      (else (+ (term a) (sum-general term (next a) next b))))))

(define another-sum-int
  (lambda (a b)
    (define identity
      (lambda (a)
        a))
    (define plus1
      (lambda (a)
        (+ 1 a)))
    (sum-general identity a plus1 b)))