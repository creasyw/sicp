#lang racket
(require "symbolic_diff.rkt")

(define (new-sum . xs)
  (cond ((null? xs) 0)
        ((= (length xs) 1) (car xs))
        (#t (apply new-sum (cons (make-sum (car xs) (cadr xs)) (cddr xs))))))

(define (new-product . xs)
    (cond ((null? xs) 1)
        ((= (length xs) 1) (car xs))
        (#t (apply new-product (cons (make-product (car xs) (cadr xs)) (cddr xs))))))