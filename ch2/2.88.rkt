#lang racket
;; it's easy to solve:
;; sub -> add the negate of the second term
;;     -> negate = multiply the poly with -1.
;; refer the solution in 2.87.rkt

(require "2.87.rkt")
(install-polynomial-package)
(install-number-package)

(define ca (make-polynomial 'x '((2 2) (1 3) (0 1))))
(define cb (make-polynomial 'x '((3 4) (1 1))))

(sub ca ca)
; '(polynomial x)
(sub cb ca)
; '(polynomial x (3 4) (2 -2) (1 -2) (0 -1))
