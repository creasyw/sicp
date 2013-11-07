#lang racket

(require "symbolic_diff.rkt")

(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
        ((= exponent 1) base)
        (#t (list base '** exponent))))

(define (exponential? x)
  (and (pair? x) (eq? (cadr x) '** )))

(define (base exp) (car exp))
(define (exponent exp) (caddr exp))

(define (new-deriv exp var)
  (cond ((exponential? exp)
         (letrec ((n (exponent exp))
                  (b (base exp)))
           (make-product n (make-product
                            (make-exponentiation b (- n 1))
                            (new-deriv b var)))))
        (#t (deriv exp var))))


;; test
(new-deriv '(x ** 0) 'x)
(new-deriv '(x ** 5) 'x)