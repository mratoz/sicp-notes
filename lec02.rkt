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

(define another-sum-square
  (lambda (a b)
    (define plus1
      (lambda (x)
        (+ 1 x)))
    (sum-general square a plus1 b)))

;;(sum-square 1 100)
;;(another-sum-square 1 100)

(define another-pi-sum
  (lambda (a b)
    (sum-general (lambda (i) (/ 1 (* i (+ i 2)))) a (lambda (i) (+ 4 i)) b)))
;;(pi-sum 1 100)
;;(another-pi-sum 1 100)

(define another-another-sum-int
  (lambda (a b)
    (sum-general (lambda (i) i) a (lambda (i) (+ 1 i)) b)))
 
;;(sum-int 1 1000)
;;(another-sum-int 1 1000)
;;(another-another-sum-int 1 1000)

(define another-another-sum-square
  (lambda (a b)
    (sum-general (lambda (i) (* i i)) a (lambda (i) (+ 1 i)) b)))

;;(sum-square 1 1000)
;;(another-sum-square 1 1000)
;;(another-another-sum-square 1 1000)


;;square root
(define fixed-point
  (lambda (f start)
    (define tolerance 0.00001)
    (define close-enuf?
      (lambda (u v)
        (< (abs (- u v)) tolerance)))
    (define iter
      (lambda (old new)
        (cond
          ((close-enuf? old new) new)
          (else (iter new (f new))))))
    (iter start (f start))))

(define sqrt-f
  (lambda (x)
    (fixed-point
     (lambda (y) (/ (+ (/ x y) y) 2)) 1)))
(trace sqrt-f)

(define sqrt-f1
  (lambda (x)
    (define avg-damp
      (lambda (f)
        (lambda (x) (/ (+ (f x) x) 2))))
    (fixed-point
     (avg-damp (lambda (y) (/ x y))) 1)))

;;square root in newton's method
(define sqrt-n
  (lambda (x)
    (newton (lambda (y) (- x (square y))) 1)))

(define newton
  (lambda (f guess)
    (define df
      (deriv f))
    (fixed-point (lambda (x) (- x (/ (f x) (df x)))) guess)))

(define deriv
  (lambda (f)
    (lambda (x)
      (/ (- (f (+ x dx)) (f x)) dx))))
(define dx 0.00001)

;;compound data
(define make-rat
  (lambda (n d)
    (let ([g (gcd n d)])
    (cons (/ n g) (cons (/ d g) '())))))
(define number
  (lambda (rat)
    (car rat)))
(define denom
  (lambda (rat)
    (car (cdr rat))))

(define +rat
  (lambda (x y)
  (make-rat
    (+ (* (number x) (denom y))
       (* (number y) (denom x)))
    (* (denom x) (denom y)))))

(define *rat
  (lambda (x y)
    (make-rat
      (* (number x) (number y))
      (* (denom x) (denom y)))))

;;(define a (make-rat 1 2))
;;(define b (make-rat 3 4))
;;(+rat a b)
;;(*rat a b)

(define make-rat-v2
  (lambda (n d)
    (cons n (cons d '()))))
(define number-v2
  (lambda (rat)
    (let ([g (gcd (car rat) (car (cdr rat)))])
      (/ (car rat) g))))
(define denom-v2
  (lambda (rat)
    (let ([g (gcd (car rat) (car (cdr rat)))])
      (/ (car (car rat)) g))))

;;(define a-v2 (make-rat-v2 1 2))
;;(define b-v2 (make-rat-v2 6 8))
;;(number-v2 a-v2)
;;(number-v2 b-v2)
;;(number b-v2)


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

(define a (make-vector 1 2))
(define b (make-vector 5 6))
(define seg-a-b (make-seg a b))
(mid-point seg-a-b)
(length seg-a-b)