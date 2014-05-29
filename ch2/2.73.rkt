#lang racket

(require "complex_number.rkt")

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
               var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; a) The data representation in this flow cannot be either a number
;; or a variable, because the operator cannot operate on them.
