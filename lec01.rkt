#lang racket
(require racket/trace)

(define square
  (lambda (x)
    (* x x)))

(square (square 1001))
square

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

(define mean-square
  (lambda (x y)
    (average (square x)
             (square y))))
(mean-square 3 4)

(define abs
  (λ (x)
    (cond
      ((< x 0) (- x))
      (else x))))
(abs 0)
(abs -1)
(abs 1)

(define SOS
  (lambda (x y)
    (+ (square x) (square y))))

(define plus
  (lambda (x y)
    (cond
      ((= x 0) y)
      (else (plus (+ -1 x) (+ 1 y))))))

(trace plus)
(plus 3 4)

(define plus-another
  (lambda (x y)
    (cond
      ((= x 0) y)
      (else (+ 1 (plus-another (+ -1 x) y))))))

(trace plus-another)
(plus-another 3 4)