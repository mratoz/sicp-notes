#lang racket
(require racket/trace)


(define fact
  (lambda (n) 
    (define iter
      (lambda (m i)
        (cond
          ((> i n) m)
          (else (iter (* i m) (+ i 1))))))
    (trace iter)
    (iter 1 1)))

(trace fact)
(fact 3)


(define fact-imp
  (lambda (n)
    (let ((i 1) (m 1))
      (define loop
        (lambda ()
          (cond
            ((> i n) m)
            (else
             (set! m (* i m))
             (set! i (+ i 1))
             (loop)))))
      (trace loop)
      (loop))))

(trace fact-imp)

(fact-imp 3)
(fact-imp 4)


(define make-counter
  (lambda (n)
    (lambda ()
      (set! n (+ n 1))
      n)))

(define c1 (make-counter 0))

(define c2 (make-counter 10))

(c1)
(c1)
(c2)
(c2)

;; Alonzo Church
(define ac-cons
  (lambda (x y)
    (lambda (m)
      (m x y))))

(define ac-car
  (lambda (x)
    (x (lambda (a b) a))))

(define ac-cdr
  (lambda (x)
    (x (lambda (a b) b))))

(ac-car (ac-cons 3 4))
(ac-cdr (ac-cons 3 4))


;; enhanced alonzo church cons with set permission
(define eac-cons
  (lambda (x y)
    (lambda (m)
      (m x
         y
         (lambda (n) (set! x n))
         (lambda (n) (set! y n))))))

(define eac-car
  (lambda (x)
    (x (lambda (a b sa sb) a))))

(define eac-cdr
  (lambda (x)
    (x (lambda (a b sa sb) b))))

(define eac-set-car!
  (lambda (x n)
    (x (lambda (a b sa sb) (sa n)))))

(define eac-set-cdr!
  (lambda (x n)
    (x (lambda (a b sa sb) (sb n)))))

(define l1 (eac-cons 3 4))
(eac-car l1)
(eac-set-car! l1 5)
(eac-car l1)

(eac-cdr l1)
(eac-set-cdr! l1 2)
(eac-cdr l1)

