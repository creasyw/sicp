#lang racket

(require "generic_arithmetic.rkt")

(define =zero? (lambda (x) (apply-generic '=zero? x)))

;; code below is for testing.
;; the implementation refers to generic_arithmetic.rkt.
(display "Testing the custom-number package...")
(install-number-package)
(eq? #t (=zero? (make-number 0)))
(eq? #f (=zero? (make-number 2)))
