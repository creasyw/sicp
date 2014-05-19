#lang racket

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; constructor
(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (#t (list a1 '+ a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (#t (list m1 '* m2))))

;; predicate
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
;; selector
(define (addend x) (car x))
(define (augend s) (caddr s))
(define (multiplier x) (car x))
(define (multiplicand x) (caddr x))

;; it's easier to add one more block of function in the "stratified
;; design" diagram rather than changing all predicate, selector, and
;; constructor as the question suggested.
;; This function will check the validity of input expression and add
;; all necessary paratheses.
(define (parse exp)
  (cond ((sum? exp)
         (if (<= (length exp) 3)
             exp
             (append (take exp 2) (list (parse (drop exp 2))))))
        ((product? exp)
         (cond ((list? (multiplicand exp))
                (append (take exp 2) (list (parse (multiplicand exp))) (drop exp 3)))
               ((<= (length exp) 3) exp)
               (#t (parse (append (list (take exp 3)) (drop exp 3))))))
        
        (#t (error "unknown expression type -- DERIV" exp))))

;; test parse
;(parse '(x + 3 * (x + y + 2)))
;(parse '(3 * x + 3 * ( x + y + 3 * 2)))

;; delete the type checking of "unknown check" in deriv
;; and move it to the parse procedure
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))))

(define (std_deriv exp var)
  (deriv (parse exp) var))

;; test deriv
(deriv '(x + (3 * (x + (y + 2)))) `x)
(std_deriv '(x + 3 * (x + y + 2)) 'x)
(std_deriv '(3 * x + 3 * ( x + y + 3 * 2)) 'x)