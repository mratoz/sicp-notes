#lang racket
(provide square average)
(provide make-vector xcor ycor)

(define square
  (lambda (x)
    (* x x)))
(define average
  (lambda (x y)
    (/ (+ x y) 2)))

;;lec2b: vector
(define make-vector
  (lambda (x y)
    (cons x (cons y '()))))

(define xcor
  (lambda (p)
    (car p)))
(define ycor
  (lambda (p)
    (car (cdr p))))

(define make-seg
  (lambda (p q)
    (cons p (cons q '()))))
(define seg-start
  (lambda (s)
    (car s)))
(define seg-end
  (lambda (s)
    (car (cdr s))))

(define mid-point
  (lambda (s)
    (let ([a (seg-start s)] [b (seg-end s)])
      (make-vector (average (xcor a) (xcor b)) (average (ycor a) (ycor b))))))

(define length
  (lambda (s)
    (let ([dx (- (xcor (seg-end s)) (xcor (seg-start s)))]
          [dy (- (ycor (seg-end s)) (ycor (seg-start s)))])
      (sqrt (+ (square dx) (square dy))))))
