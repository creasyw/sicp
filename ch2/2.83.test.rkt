#lang racket

(require "2.83.rkt")

(install-complex-package)
(install-number-package)
(install-rational-package)

;; basic raise
(apply-generic 'equ
               (make-rational 10 1)
               (apply-to-two 'raise (make-number 10)))
(apply-generic 'equ
               (make-number 5.0)
               (apply-to-two 'raise (make-rational 10 2)))
(apply-generic 'equ
               (make-complex-from-real-imag 3.0 0)
               (apply-to-two 'raise (make-number 3.0)))
;; basic operation upone one or two arguments
(apply-generic 'equ
               (make-number 6)
               (apply-generic 'mul (make-number 2) (make-number 3)))
(apply-generic 'equ
               (make-rational 1 10)
               (apply-generic 'mul (make-rational 1 5) (make-rational 1 2)))
(apply-generic 'equ
               (make-complex-from-mag-ang 10.0 2.0)
               (apply-generic 'mul (make-number 5) (make-complex-from-mag-ang 2 2)))

;; operation upon multiple arguments
(apply-generic 'equ
               (make-rational 98 11)
               (apply-generic 'add (make-number 5) (make-rational 10 11)(make-number 3)))
(apply-generic 'equ
               (make-rational 17 3)
               (apply-generic 'add (make-rational 2 3) (make-rational 5 3) (make-rational 10 3)))
(apply-generic 'equ
               (make-complex-from-real-imag 18.0 3.0)
               (apply-generic 'mul (make-rational 3 1) (make-number 1) (make-complex-from-real-imag 6 1)))
