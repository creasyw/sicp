#lang racket

(define x (list 1 2 3))
(define y (list 4 5 6))

(equal? (append x y)
        (list 1 2 3 4 5 6))
(equal? (cons x y)
        (list (list 1 2 3) 4 5 6))
(equal? (list x y)
        (list (list 1 2 3) (list 4 5 6)))
