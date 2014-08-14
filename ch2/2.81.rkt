;; a.
;; Louis's solution would generate infinite loop if the "'proc" does
;; not define in the given package. On the other hand, if the "'proc"
;; has been defined, the same-type coercion will never be called.

;; b.
;; No. The original implementation of apply-generic is correct.
;; Refer to the comments in generic_arithmetic.rkt for more info.

;; c.
;; Refer to generic_arithmetic.rkt for implementation.
;; for testing
#lang racket

(require "generic_arithmetic.rkt")
(install-number-package)
(install-rational-package)
(install-complex-package)

(apply-generic 'add (make-number 3) (make-number 5))
(apply-generic 'sub (make-rational 5 2) (make-rational 10 21))
(apply-generic 'mul (make-number 5) (make-complex-from-mag-ang 2 3))
;; the problematic one
;(apply-generic 'exp (make-number 3) (make-number 5))
