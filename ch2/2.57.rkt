#lang racket
(require "symbolic_diff.rkt")

;; The design of the program makes it possible to accept multiple
;; input parameters via only changing the sum and product:
;; The higher-level deriv has _no_ assumption about the number of
;; input parameters, all operations are defined in a recursive manner.
;; The lower-level of data representation also have no idea (and don't
;; care about) how many operators are in a formula
(define (new-sum . xs)
  (cond ((null? xs) 0)
        ((= (length xs) 1) (car xs))
        (#t (apply new-sum (cons (make-sum (car xs) (cadr xs)) (cddr xs))))))

(define (new-product . xs)
    (cond ((null? xs) 1)
        ((= (length xs) 1) (car xs))
        (#t (apply new-product (cons (make-product (car xs) (cadr xs)) (cddr xs))))))
