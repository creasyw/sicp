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
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (addend opds) (car opds))
  (define (augend opds) (cadr opds))
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (multiplier opds) (car opds))
  (define (multiplicand opds) (cadr opds))
  (define (deriv-product exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))
  
  ;; implementation of deriv-exponential
  ;; predicate
  (define (make-exponentiation base exp)
    (cond ((=number? exp 0) 1)
          ((=number? exp 1) base)
          (else (list '** base exp))))
  (define (base opds) (car opds))
  (define (exponent opds) (cadr opds))
  (define (deriv-exponential opds var)
    (make-product
     (exponent opds)
     (make-product
      (make-exponentiation (base opds)
                           (make-sum (exponent opds) (- 1)))
      (deriv (base opds) var))))
  
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

; tests
;(install-deriv-package)
;(deriv '(** x 4) 'x)
;; Value: (* 4 (** x 3))
;(deriv '(** x n) 'x)
;; Value: (* n (** x (+ n -1)))
;(deriv '(+ (* 3 x) y) 'x)
;; Value: 3
;(deriv '(** (+ (* 3 x) y) 1) 'x)
;; Value: 3
;(deriv '(** (+ (** x 2) 1) 2) 'x)
;; Value: (* 2 (* (+ (** x 2) 1) (* 2 x)))