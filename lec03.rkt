#lang racket
(require "base.rkt")
(require racket/trace)

(define +vect
  (lambda (v1 v2)
    (make-vector
     (+ (xcor v1) (xcor v2))
     (+ (ycor v1) (ycor v2)))))


;;(define v1 (make-vector 1 2))
;;(define v2 (make-vector 3 4))
;;(+vect v1 v2)

(define scale
  (lambda (s v)
    (make-vector
     (* s (xcor v))
     (* s (ycor v)))))

;;(define v1 (make-vector 1 2))
;;(scale 2 v1)

;;list
(define 1-to-4 (list 1 2 3 4))
;;(car 1-to-4)
;;(car (cdr 1-to-4))
;;(car (cdr (cdr 1-to-4)))
;;(car (cdr (cdr (cdr 1-to-4))))
;;error
;;(car (cdr (cdr (cdr (cdr 1-to-4)))))
;;(cdr (cdr (cdr (cdr 1-to-4))))
;;error
;;(cdr (cdr (cdr (cdr (cdr 1-to-4)))))

(define scale-list
  (lambda (s l)
    (cond
      ((null? l) '())
      (else (cons (* s (car l)) (scale-list s (cdr l)))))))

(scale-list 10 1-to-4)

;;general pattern of map
(define map
  (lambda (p l)
    (cond
      ((null? l) '())
      (else (cons (p (car l)) (map p (cdr l)))))))

(define scale-list-via-map
  (lambda (s l)
    (map (lambda (i) (* s i)) l)))

(scale-list-via-map 10 1-to-4)

(map (lambda (i) (+ 10 i)) 1-to-4)

(define filter
  (lambda (f l)
    (cond
      ((null? l) '())
      ((equal? (f (car l)) #t) (cons (car l) (filter f (cdr l))))
      (else (filter f (cdr l))))))

(filter (lambda (i) (> i 3)) 1-to-4)
(filter (lambda (i) (> i 3)) (map (lambda (i) (+ 10 i)) 1-to-4))
