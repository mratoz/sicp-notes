#lang racket
(require "base.rkt")
(require racket/trace)

(define deriv
  (lambda (exp var)
    (cond
      ((constant? exp var) 0)
      ((same-var? exp var) 1)
      ((sum? exp)
       (make-sum (deriv (A1 exp) var)
                 (deriv (A2 exp) var)))
      ((product? exp)
       (make-sum (make-product (M1 exp)
                               (deriv (M2 exp) var))
                 (make-product (deriv (M1 exp) var)
                               (M2 exp))))
      (else `(not considered yet)))))

(define constant?
  (lambda (exp var)
    (and (atom? exp)
         (not (eq? exp var)))))

(define same-var?
  (lambda (exp var)
    (and (atom? exp)
         (eq? exp var))))

(define sum?
  (lambda (exp)
    (and (not (atom? exp))
         (eq? (car exp) `+))))

(define make-sum
  (lambda (a1 a2)
    (cond
      ((and (number? a1)
            (number? a2))
       (+ a1 a2))
      ((and (number? a1)
            (eq? a1 0))
       a2)
      ((and (number? a2)
            (eq? a2 0))
       a1)
      (else (list `+ a1 a2)))))
;;    (list `+ a1 a2)))

(define A1 cadr)
(define A2 caddr)

(define product?
  (lambda (exp)
    (and (not (atom? exp))
         (eq? (car exp) '*))))

(define make-product
  (lambda (m1 m2)
    (cond
      ((and (number? m1)
            (number? m2))
       (* m1 m2))
      ((eq? m1 1) m2)
      ((eq? m2 1) m1)
      ((or (eq? m1 0) (eq? m2 0)) 0)
      (else (list `* m1 m2)))))
    
;;    (list `* m1 m2)))

(define M1 cadr)
(define M2 caddr)


(define foo
  `(+ (* a (* x x))
     (+ (* b x)
        c)))

(trace deriv)
(trace sum?)
(trace product?)
(trace constant?)
(trace same-var?)

(deriv foo `x)
;;(deriv foo `a)
;;(deriv foo `b)
;;(deriv foo `c)