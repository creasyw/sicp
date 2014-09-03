#lang racket

(require "2.87.rkt")
(install-polynomial-package)
(install-number-package)

(define ca (make-polynomial 'x '((2 2) (1 3) (0 1))))
(define cb (make-polynomial 'x '((3 4) (1 1))))
(define cc (make-polynomial 'x '((1 1) (0 10))))
(define cd (make-polynomial 'x '((3 -4) (1 -1))))

;; same poly
(div ca ca) ; '((polynomial x (0 1)) (polynomial x))
;; divident has smaller order
(div ca cb) ; '((polynomial x) (polynomial x (2 2) (1 3) (0 1)))
;; divident has higher order
(div cb ca) ; '((polynomial x (1 2) (0 -3)) (polynomial x (1 8) (0 3)))
