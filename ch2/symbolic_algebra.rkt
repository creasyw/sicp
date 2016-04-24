#lang racket

;; import get, put
(require "complex_num_table.rkt")

(provide (all-defined-out))

;; retrieve the generic functions from the table
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (install-polynomial-package)
  ;; higher-level constructor and selectors
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? p1 p2)
    (eq? (variable p1) (variable p2)))

  ;; constructors and selectors for terms
  (define (the-empty-termlist) '())
  (define (empty-termlist? l) (null? l))
  (define (make-term order coeff) (list order coeff))
  (define (first-term l) (car l))
  (define (rest-terms l) (cdr l))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  ;; operations for terms
  (define (negate term-list)
    (define (helper acc tlst)
      (if (empty-termlist? tlst)
          acc
          (let ((minus-one (make-term 0 -1)))
            (helper (adjoin-term (mul-terms minus-one (car tlst)) acc) (cdr tlst)))))
    (helper '() term-list))

  (define (adjoin-term term term-list)
    ;; append term to the end of term-list
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (add-terms l1 l2)
    (cond ((empty-termlist? l1) l2)
          ((empty-termlist? l2) l1)
          (else
           (letrec ((t1 (first-term l1))
                    (t2 (first-term l2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1 (add-terms (rest-terms l1) l2)))
                   ((< (order t1) (order t2))
                    (adjoin-term t2 (add-terms l1 (rest-terms l2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms l1) (rest-terms l2)))))))))

  ;; multiplication is built on the addition.
  (define (mul-terms l1 l2)
    (if (empty-termlist? l1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term l1) l2)
                   (mul-terms (rest-terms l1) l2))))

  (define (mul-term-by-all-terms t1 l2)
    (if (empty-termlist? l2)
        (the-empty-termlist)
        (let ((t2 (first-term l2)))
          (adjoin-term
           (make-term (add (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms l2))))))

  ;; regular operations
  (define (add-poly p1 p2)
    (if (same-variable? p1 p2)
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY" (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? p1 p2)
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY" (list p1 p2))))

  ;; interface to the rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial)
       (lambda (p) (empty-termlist? (term-list p))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
)

;; interface for the package
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;; ===========================

;; ordinary number
;; tag: custom-number
(define (install-number-package)
  (define (tag x)
    (attach-tag 'custom-number x))
  (put 'add '(custom-number custom-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(custom-number custom-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(custom-number custom-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(custom-number custom-number)
       (lambda (x y) (tag (/ x y))))
  (put 'square-root '(custom-number)
       (lambda (x) (tag (sqrt x))))
  (put 'sine '(custom-number)
       (lambda (x) (tag (sin x))))
  (put 'cosine '(custom-number)
       (lambda (x) (tag (cos x))))
  (put 'atangent '(custom-number custom-number)
       (lambda (x y) (tag (atan x y))))
  (put 'equ '(custom-number custom-number) =)
  (put '=zero? '(custom-number)
       (lambda (x) (= x 0)))
  (put 'make 'custom-number
       (lambda (x) (tag x)))
  'done)
;; constructor.
(define (make-number n)
  ((get 'make 'custom-number) n))

;; tagging functions
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'custom-number)
        (#t (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (#t (error "Bad tagged datum -- CONTENTS" datum))))
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

;; interface for the package
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types"
                 (list op type-tags))))))
