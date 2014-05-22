#lang racket

(provide union-set)

;; because both intersection and union build on the assumption that
;; items of sets are in increasing order, they do not need the func
;; element-of-set? anymore.
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (#t (letrec ((x1 (car s1))
                     (x2 (car s2)))
              (cond ((= x1 x2) (cons x1 (union-set (cdr s1) (cdr s2))))
                    ((< x1 x2) (cons x1 (union-set (cdr s1) s2)))
                    (#t (cons x2 (union-set s1 (cdr s2)))))))))

;; test union
;(union-set (list 1 2 3) (list 2 4 5))
;(union-set '() '())
;(union-set '() (list 1 2 3))
;(union-set (list 1 2 3) '())
