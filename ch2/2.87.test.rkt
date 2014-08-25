#lang racket

(require "2.87.rkt")
(install-polynomial-package)
(install-number-package)

(define ca (make-polynomial 'x '((2 2) (1 3) (0 1))))
(define cb (make-polynomial 'x '((3 4) (1 1))))
(define cc (make-polynomial 'x '((1 1) (0 10))))
(define cd (make-polynomial 'x '((3 -4) (1 -1))))

(add ca cb)
; (polynomial x (3 4) (2 2) (1 4) (0 1))
(=zero? (add cb cd))
; #t
(mul ca cc)
; (polynomial x (3 2) (2 23) (1 31) (0 10))
