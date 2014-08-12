#lang racket

(require "generic_arithmetic.rkt")

(define equ? (lambda (x y) (apply-generic 'equ x y)))

;; The tag of 'equ has been implemented in the generic_arithmetic.rkt.
;; The following code is for testing.
(display "Testing number package...")
(install-number-package)
(eq? #t (equ? (make-number 5) (make-number 5)))
(eq? #f (equ? (make-number 5) (make-number 3)))

(display "Testing rational package...")
(install-rational-package)
(eq? #t (equ? (make-rational 2 5) (make-rational 2 5)))
(eq? #t (equ? (make-rational 2 5) (make-rational 4 10)))
(eq? #f (equ? (make-rational 2 5) (make-rational 2 3)))

(display "Testing complex package... ")
(install-complex-package)
(eq? #t (equ? (make-complex-from-mag-ang 1 2) (make-complex-from-mag-ang 1 2)))
(eq? #t (equ? (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2)))
(eq? #f (equ?(make-complex-from-mag-ang 1 2) (make-complex-from-mag-ang 2 2)))
;; equ involving coercion
(eq? #t (equ? (make-complex-from-real-imag 2 0) (make-number 2)))
