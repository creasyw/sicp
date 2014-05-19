#lang racket

(provide element-of-set?)

(define (element-of-set? x s1)
  (cond ((null? s1) false)
        ((equal? x (car s1)) true)
        ((< x (car s1)) false)
        (#t (element-of-set? x (cdr s1)))))

(define (intersection-set s1 s2)
  (if (or (null? s1) (null? s2)) '()
      (let ((x1 (car s1))
            (x2 (car s2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr s1) (cdr s2))))
              ((< x1 x2) (intersection-set (cdr s1) s2))
              (#t (intersection-set s1 (cdr s2)))))))

;; test intersection
;(intersection-set (list 1 2 3) (list 2 4 5))
;(intersection-set '() '())
;(intersection-set '() (list 1 2 3))
;(intersection-set (list 1 2 3) '())

(define (adjoin-set x s1)
  (define (insert acc)
    (cond ((null? acc) (append s1 (list x)))
          ((< (car acc) x) (insert (cdr acc)))
          ((= (car acc) x) s1)
          (#t (append (take s1 (- (length s1) (length acc))) (list x) acc))))
  (insert s1))

;; test adjoin-set
;(adjoin-set 5 (list 1 2 3))
;(adjoin-set -4 (list 1 2 3))
;(adjoin-set 2 (list 1 2 3))
;(adjoin-set 5 '())