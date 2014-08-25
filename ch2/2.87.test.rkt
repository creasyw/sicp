#lang racket

(require "2.87.rkt")
(install-polynomial-package)
(install-number-package)

(define ca (make-polynomial 'x '((2 2) (1 3) (0 1))))
(define cb (make-polynomial 'x '((3 4) (1 1))))

(add ca cb)
; (polynomial x (3 4) (2 2) (1 4) (0 1))
