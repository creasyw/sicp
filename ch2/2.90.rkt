#lang racket
;; This question is similar with/different from the two
;; representations of complex numbers. It's necessary to use two
;; "make-complex" functions, but not for poly. In this file, the
;; "make-poly" is still one unified function, and it will decide which
;; representation the input is and choose corresponding package.

;; For operations upon two different representations, it's reasonable
;; to use the shorter one, but in the code below, I use a simplified
;; version which will unify all representations to the 1st
;; polynomial. Accordingly, the method "transform" is implemented in
;; both packages.

;; import get, put
(require "complex_num_table.rkt")

(provide (all-defined-out))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (install-sparse-package)
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
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  ;; predicate
  (define (empty-termlist? term-list) (null? term-list))
  ;; constructor and selector for "sparse order of polynomial"
  (define (the-empty-termlist) '())
  (define (make-term order coeff) (list order coeff))
  (define (first-term term-list) (car term-list))
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
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

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
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (transform-to-dense var terms)
    (define (to-dense acc digits lst)
      (cond ((< digits 0) (reverse acc))
            ((= 0 (length lst)) (to-dense (cons 0 acc) (- digits 1) lst))
            ((= digits (car (car lst))) (to-dense (cons (cadr (car lst)) acc) (- digits 1) (cdr lst)))
            (#t (error "The terms are wrongly given" lst))))
    (attach-tag 'dense (cons var (to-dense '() (car (car terms)) terms))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'sparse p))
  (put 'add '(sparse sparse)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(sparse sparse)
       (lambda (p1 p2) (tag (add-poly p1 (negate p2)))))
  (put 'mul '(sparse sparse)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(sparse)
       (lambda (p) (empty-termlist? (term-list p))))
  (put 'make 'sparse
       (lambda (var terms) (tag (make-poly var terms))))

  (put 'transform 'sparse
       (lambda (p) (transform-to-dense (variable p) (term-list p))))

  'done)

;; for dense representation
(define (install-dense-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (variable? p) (symbol? p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

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
    (define (remove-leading-zeros lst)
      (cond ((null? lst) lst)
            ((= 0 (car lst)) (remove-leading-zeros (cdr lst)))
            (#t lst)))
    (define (helper)
      (cond ((empty-termlist? L1) L2)
            ((empty-termlist? L2) L1)
            (#t (letrec ((len1 (length L1))
                         (len2 (length L2)))
                  (cond ((> len1 len2) (append (take L1 (- len1 len2))
                                               (list-add (drop L1 (- len1 len2)) L2)))
                        ((< len1 len2) (append (take L2 (- len2 len1))
                                               (list-add L1 (drop L2 (- len2 len1)))))
                        (#t (list-add L1 L2))))))  )
    (remove-leading-zeros (helper)))

  ;; for subtraction
  (define (negate p1)
    (mul-poly p1 (make-poly (variable p1) '(-1))))

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

  (define (transform-to-sparse var terms)
    (define (to-sparse acc lst)
      (cond ((= 0 (length lst)) acc)
            ((= 0 (car lst)) (to-sparse acc (cdr lst)))
            (#t (to-sparse (append acc (list (list (- (length lst) 1) (car lst)))) (cdr lst)))))
    (attach-tag 'sparse (cons var (to-sparse '() terms))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'dense p))
  (put 'add '(dense dense)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(dense dense)
       (lambda (p1 p2) (tag (add-poly p1 (negate p2)))))
  (put 'mul '(dense dense)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(dense)
       (lambda (p) (empty-termlist? (term-list p))))
  (put 'make 'dense
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-polynomial var terms)
  (if (list? (car terms))
      ((get 'make 'sparse) var terms)
      ((get 'make 'dense) var terms)))

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
  (letrec ((type-tags (map type-tag args))
           (new-args (for/list ([type type-tags]
                                [arg args])
                       (if (or (eq? type 'custom-number) (eq? type (car type-tags)))
                           arg
                           ((get 'transform type) (contents arg)))))
           (new-type-tags (build-list (length type-tags) (lambda (x) (car type-tags))))
           (proc (get op new-type-tags)))
    (if proc
        (apply proc (map contents new-args))
        (error "No method for these types" (list op type-tags)))))

;; for testing
(install-sparse-package)
(install-dense-package)
(install-number-package)
(make-polynomial 'x '(2 3 1))
(make-polynomial 'y '((2 2) (1 3) (0 1)))

(define ca (make-polynomial 'x '((2 2) (1 3) (0 1))))
(define cb (make-polynomial 'x '((3 4) (1 1))))
(define cc (make-polynomial 'x '((1 1) (0 10))))
(define cd (make-polynomial 'x '(4 0 1 0)))


(displayln "\nAdding two sparse together:")
(add ca cb)
; '(sparse x (3 4) (2 2) (1 4) (0 1))
(add cd cd)
; '(dense x 8 0 2 0)
(mul ca cb)
; '(sparse x (5 8) (4 12) (3 6) (2 3) (1 1))
(mul cd cd)
; '(dense x 16 0 8 0 1 0 0)

(displayln "\nAdding a sparse with dense:")
(add cd ca)
; '(dense x 4 2 4 1)
