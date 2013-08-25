#lang racket

(define (my-equal? lst1 lst2)
  (or (and (null? lst1) (null? lst2))
      (and (eq? (car lst1) (car lst2))
           (my-equal? (cdr lst1) (cdr lst2)))))

;; testing
(my-equal? (list 1 2 3) (list 1 2 3))
(my-equal? (list 1 2 (list 3 4) 5) (list 1 2 3 4 5))