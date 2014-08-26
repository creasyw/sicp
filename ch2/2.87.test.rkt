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

(define p1 (make-polynomial 'y (list (list 2 ca)
                                     (list 1 cb)
                                     (list 0 cc))))
(define p2 (make-polynomial 'y (list (list 1 ca)
                                     (list 0 cb))))
(define p3 (make-polynomial 'y (list (list 2 ca)
                                     (list 1 cd)
                                     (list 0 cc))))

(add p1 p2)
; (polynomial y (2 (polynomial x (2 2) (1 3) (0 1)))
;               (1 (polynomial x (3 4) (2 2) (1 4) (0 1)))
;               (0 (polynomial x (3 4) (1 2) (0 10))))
(add p1 p3)
; (polynomial y (2 (polynomial x (2 4) (1 6) (0 2)))
;               (0 (polynomial x (1 2) (0 20))))
(mul p2 p3)
; (polynomial y (3 (polynomial x (4 4) (3 12) (2 13) (1 6) (0 1)))
;               (1 (polynomial x (6 -16) (4 -8) (3 2) (2 22) (1 31) (0 10)))
;               (0 (polynomial x (4 4) (3 40) (2 1) (1 10))))
