#lang racket

(require "2.90.rkt")

;; for testing
(install-sparse-package)
(install-dense-package)
(install-number-package)
(make-polynomial 'x '(2 3 1))
(make-polynomial 'y '((2 2) (1 3) (0 1)))

(define ca (make-polynomial 'x '((2 2) (1 3) (0 1))))
(define cb (make-polynomial 'x '((3 4) (1 1))))
(define cc (make-polynomial 'x '((1 1) (0 10))))
(define cd (make-polynomial 'x '(4 0 1 0)))


(displayln "\nAdding two sparse together:")
(add ca cb)  ; '(sparse x (3 4) (2 2) (1 4) (0 1))
(add cd cd)  ; '(dense x 8 0 2 0)
(mul ca cb)  ; '(sparse x (5 8) (4 12) (3 6) (2 3) (1 1))
(mul cd cd)  ; '(dense x 16 0 8 0 1 0 0)

(displayln "\nAdding a sparse with dense:")
(add cd ca)  ; '(dense x 4 2 4 1)
(add ca cd)  ; '(sparse x (3 4) (2 2) (1 4) (0 1))
