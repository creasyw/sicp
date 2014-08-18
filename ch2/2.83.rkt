#lang racket

;; this is a modified version of generic_arithmetic.rkt.
;; there is no explicit coercion. The "raise" and "drop" are
;; implemented instead. Besides, the additional considerations are:
;; 1. decide which data type has relatively higher rank. (2.84)
;; 2. "compatible" with later installed new level. (2.84 and 2.85)
;; 3. dealing with more than two arguments

;; import get, put
(require "complex_num_table.rkt")


(provide (all-defined-out))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

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
  (put 'equ '(custom-number custom-number) =)
  (put '=zero? '(custom-number)
       (lambda (x) (= x 0)))

  (put 'make 'custom-number
       (lambda (x) (tag x)))
  ;; raise -- either from integer to rational or real to complex
  (put 'raise '(custom-number)
       (lambda (x) (if (exact-integer? x)
                       (make-rational x 1)
                       (make-complex-from-real-imag x 0))))

  'done)
;; constructor.
(define (make-number n)
  ((get 'make 'custom-number) n))


;; rational number
(define (install-rational-package)
  ;; internal procedures
  ;; basic dispatch
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  ;; setter
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  ;; getter
  (define (get-rat x)
    (/ (exact->inexact (numer (cdr x))) (denom (cdr x))))
  ;; basic operations
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  ;; put anonymous functions into the table of rational number
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y))
                          (= (denom x) (denom y)))))
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'get 'rational
       (lambda (x) (get-rat x)))
  ;; raise rational to real number
  (put 'raise '(rational)
       (lambda (x) (make-number (get-rat x))))

  (define (number->rational n) (make-rat n 1))

  (put 'custom-number 'rational
       (lambda (x) (tag (number->rational x))))

  'done)

;; constructor
(define (make-rational n d)
  ((get 'make 'rational) n d))
;; selector of rational number
(define (get-rational n)
  ((get 'get 'rational) n))

;; complex number
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; basic functions relevant with storing data
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (attach-tag 'rectangular (cons x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (attach-tag 'polar (cons r a))))

  (define (real-part z)
    (cond ((eq? (car z) 'rectangular) (car (cdr z)))
          ((eq? (car z) 'polar)
           (* (magnitude z) (cos (angle z))))
          (#t (error "The tag of z is unacceptable--REAL_PART: " z))))
  (define (imag-part z)
    (cond ((eq? (car z) 'rectangular) (cdr (cdr z)))
          ((eq? (car z) 'polar)
           (* (magnitude z) (sin (angle z))))
          (#t (error "The tag of z is unacceptable--IMAG_PART: " z))))
  (define (magnitude z)
    (cond ((eq? (car z) 'polar) (car (cdr z)))
          ((eq? (car z) 'rectangular)
           (sqrt (+ (* (real-part z) (real-part z))
                    (* (imag-part z) (imag-part z)))))
          (#t (error "The tag of z is unacceptable--MAGNITUDE:" z))))
  (define (angle z)
    (cond ((eq? (car z) 'polar) (cdr (cdr z)))
          ((eq? (car z) 'rectangular)
           (atan (imag-part z) (real-part z)))
          (#t (error "The tag of z is unacceptable--ANGLE:" z))))

  ;; basic operations
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ '(complex complex)
       (lambda (x y) (and (= (real-part x) (real-part y))
                          (= (imag-part x) (imag-part y)))))
  (put '=zero? '(complex)
       (lambda (x) (and (= (real-part x) 0)
                        (= (imag-part x) 0))))

  ;; define the coercion towards complex
  (define (number->complex n)
    (make-from-real-imag (contents n) 0))
  (put 'custom-number 'complex
       (lambda (x) (tag (number->complex x))))

  (define (rational->complex n)
    (make-complex-from-real-imag (get-rational n) 0))
  (put 'rational 'complex
       (lambda (x) (rational->complex x)))

  'done)

;; constructors for both representations
(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a))


;; implementation of coercion
(define (get-coercion type1 type2) (get type1 type2))

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


;; for the tagging system, one of the tricky parts is the
;; custom-number could be either integer or real number. The former is
;; lower than rational, but the latter is higher than rational

(define (get-rank arg)
  (letrec ((tag (type-tag arg)))
    (cond ((eq? tag 'complex) 4)
          ((eq? tag 'rational) 2)
          ((eq? tag 'custom-number)
           (if (exact-integer? (contents arg)) 1 3))
          (#t (error "The tag of the arguments is incorrect:" arg)))))

(define (apply-generic op . args)
  ;; raising the arguments for specific times
  (define (raise times arg)
    (if (= times 0)
        arg
        (apply-to-two 'raise arg)))

  (letrec ((score-list (map get-rank args))
           (highest (foldl max 0 score-list))
           (new-args (for/list ([times (map (lambda (x) (- highest x)) score-list)]
                                [arg args])
                       (raise times arg))))
    (displayln new-args)
    (if (= 1 (length new-args))
        (apply-to-two op (car new-args))
        (foldl (lambda (x y) (apply-to-two op x y)) (car new-args) (cdr new-args)))))

(define (apply-to-two op . args)
  ;(displayln args)
  (letrec ((type-tags (map type-tag args))
           (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (error "There is no such operation for corresponding args:" op args)
        )))