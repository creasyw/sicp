#lang racket

;; a) The data representation in this flow cannot be either a number
;; or a variable, because the operators cannot operate on them.

(require "complex_number.rkt")
;; import get, put
(require "complex_num_table.rkt")

(define (install-deriv-package)
  
  (define (=number? exp num)
  (and (number? exp) (= exp num)))
  ;; the deriv-sum and deriv-product are actually the part of the
  ;; original deriv in the symbolic_diff.rkt. The reason to separate
  ;; them is to meet the interface provided by the question.
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (define (deriv-product exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))

  (define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          ;; the "get" actually determines what to implement
          (else ((get 'deriv (operator exp)) (operands exp)
                 var))))

  ;; implementation of deriv-exponential
  ;; predicate
  (define (exponential? x)
    (and (pair? x) (eq? (cadr x) '** )))
  (define (base exp) (car exp))
  (define (exponent exp) (caddr exp))
  (define (make-exponentiation base exponent)
    (cond ((= exponent 0) 1)
          ((= exponent 1) base)
          (#t (list base '** exponent))))
  (define (deriv-exponential exp var)
    (cond ((exponential? exp)
           (letrec ((n (exponent exp))
                    (b (base exp)))
             (make-product n (make-product
                              (make-exponentiation b (- n 1))
                              (new-deriv b var)))))
          (#t (deriv exp var))))

  (define (operator exp) (car exp))
  (define (operands exp) (cdr exp))

  ;; interface
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponential)
  
  'done)

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ;; the "get" actually determines what to implement
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))