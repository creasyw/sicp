#lang racket

(require "generic_arithmetic.rkt")

(define =zero? (lambda (x) (apply-generic '=zero? x)))

;; code below is for testing.
;; the implementation refers to generic_arithmetic.rkt.
(display "Testing the custom-number package...")
(install-number-package)
(eq? #t (=zero? (make-number 0)))
(eq? #f (=zero? (make-number 2)))

(display "Testing the rational package...")
(install-rational-package)
(eq? #t (=zero? (make-rational 0 2)))
(eq? #f (=zero? (make-rational 2 2)))

(display "Testing the complex package...")
(install-complex-package)
(eq? #t (=zero? (make-complex-from-real-imag 0 0)))
(eq? #t (=zero? (make-complex-from-mag-ang 0 0)))
(eq? #t (=zero? (make-complex-from-mag-ang 0 1)))
(eq? #f (=zero? (make-complex-from-mag-ang 1 0)))
