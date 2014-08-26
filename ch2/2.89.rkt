#lang racket

;; import get, put
(require "complex_num_table.rkt")

(provide (all-defined-out))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (variable? p) (symbol? p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (define (helper ord acc)
      (if (= order (length acc))
          (cons (coeff term) acc)
          (helper ord (cons 0 acc))))
    (if (=zero? (coeff term))
        term-list
        (helper (order term) term-list)))

  ;; predicate
  (define (empty-termlist? term-list) (null? term-list))
  ;; constructor and selector for "sparse order of polynomial"
  (define (the-empty-termlist) '())
  (define (make-term order coeff) (list order coeff))
  (define (first-term term-list) (list (- (length term-list) 1) (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  ;; for add
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Poly not in same var -- ADD-POLY"
               (list p1 p2))))
  ;; helper for add-poly
  (define (add-terms L1 L2)
    (define (list-add lst1 lst2)
      (for/list ([l1 lst1] [l2 lst2])
        (+ l1 l2)))
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (#t (letrec ((len1 (length L1))
                       (len2 (length L2)))
                (cond ((> len1 len2) (append (take L1 (- len1 len2))
                                             (list-add (drop L1 (- len1 len2)) L2)))
                      ((< len1 len2) (append (take L2 (- len2 len1))
                                             (list-add L1 (drop L2 (- len2 len1)))))
                      (#t (list-add L1 L2)))))))

  ;; for subtraction
  (define (negate p1)
    (mul-poly p1 (make-poly (variable p1) '((0 -1)))))

  ;; for multiplication
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  ;; helper function for mul-pily
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((newlist (append L (build-list (order t1) (lambda (x) 0)))))
          (map (lambda (x) (* x (coeff t1))) newlist))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 (negate p2)))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial)
       (lambda (p) (empty-termlist? (term-list p))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-polynomial var terms)
    ((get 'make 'polynomial) var terms))

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

;; testing
(install-polynomial-package)
(install-number-package)
(define ca (make-polynomial 'x '(2 3 1)))
(define cb (make-polynomial 'x '(4 0 1 0)))
(add ca cb)
