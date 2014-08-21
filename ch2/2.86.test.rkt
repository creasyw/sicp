#lang racket

(require "2.86.rkt")
(install-number-package)
(install-rational-package)
(install-complex-package)

;; testing custom-number package
(= (+ 5 1) (add (make-number 5) (make-number 1)))
(= (sqrt 10) (square-root (make-number 10)))
(= (sin 2) (sine (make-number 2)))
(= (cos 2) (cosine (make-number 2)))
(= (atan 2 2) (atangent 2 2))

;; testing rational package
(= (+ 5/2 3/2) (add (make-rational 5 2) (make-rational 3 2)))
(equ? (make-rational 4 3) (square-root (make-rational 16 9)))
(= (sqrt 4/3) (square-root (make-rational 4 3)))
