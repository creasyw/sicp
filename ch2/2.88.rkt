#lang racket
;; solution 1:
;; sub -> add the negate of the second term
;;     -> negate = multiply the poly with -1.
;;
;; solution 2: Put the negate function in the number package, which
;; actually makes more sense -- negate is to operate upon number.
;; --> Then to negate a term-list is simply done by map.

(require "symbolic_algebra.rkt")
(install-polynomial-package)
(install-number-package)

(define ca (make-polynomial 'x '((2 2) (1 3) (0 1))))
(define cb (make-polynomial 'x '((3 4) (1 1))))

(sub ca ca)
; '(polynomial x)
(sub cb ca)
; '(polynomial x (3 4) (2 -2) (1 -2) (0 -1))
