#lang racket
(provide (all-defined-out))

(define (element-of-set? x s1)
  (cond ((null? s1) false)
        ((equal? x (car s1)) true)
        (#t (element-of-set? x (cdr s1)))))

(define (adjoin-set x s1)
  (if (element-of-set? x s1)
      s1
      (cons x s1)))

(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((element-of-set? (car s1) s2)
         (cons (car s1) (intersection-set (cdr s1) s2)))
        (#t (intersection-set (cdr s1) s2))))

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((element-of-set? (car s1) s2)
         (union-set (cdr s1) s2))
        (#t (cons (car s1) (union-set (cdr s1) s2)))))

;; test
;(define x (list 1 2 3 4 5))
;(element-of-set? 3 x)
;(element-of-set? 7 x)
;(define y (list 1 2 4 5 6 7 8 9))
;(intersection-set x y)
;(union-set x y)