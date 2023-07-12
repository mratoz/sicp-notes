#lang racket
(require "base.rkt")
(require racket/trace)
(require racket/draw)

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


;;Square Limit
(define make-rect
  (lambda (origin h v)
    (cons origin (cons h (cons v '())))))

(define horiz
  (lambda (rect)
    (car (cdr rect))))

(define vert
  (lambda (rect)
    (car (cdr (cdr rect)))))

(define origin
  (lambda (rect)
    (car rect)))

(define coord-map
  (lambda (rect)
    (lambda (point)
      (+vect
       (+vect (scale (xcor point) (horiz rect))
              (scale (ycor point) (vert rect)))
       (origin rect)))))

(define make-picture
  (lambda (seglist)
    (lambda (rect)
      (for-each
       (lambda (s)
         (draw-line ; TODO: to implement in the future
          ((coord-map rect) (seg-start s))
          ((coord-map rect) (seg-end s))))
       seglist))))

(define beside
  (lambda (p1 p2 a)
    (lambda (rect)
      (p1 (make-rect
           (origin rect)
           (scale a (horiz rect))
           (vert rect)))
      (p2 (make-rect
           (+vect (origin rect)
                  (scale a (horiz rect)))
           (scale (- 1 a) (horiz rect))
           (vert rect))))))

(define right-push
  (lambda (p n a)
    (cond
      ((= n 0) p)
      (else (beside p (right-push p (- n 1) a) a)))))

(define push
  (lambda (comb)
    (lambda (pict n a)
      (
       (repeated
        (lambda (p)
          (comb pict p a))
        n)
       pict))))

(define another-right-push (push beside))

(define rotate90
  (lambda (pict)
    (lambda (rect)
      (pict (make-rect
             (+vect (origin rect)
                    (horiz rect))
             (vert rect)
             (scale -1 (horiz rect)))))))