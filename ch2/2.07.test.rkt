#lang racket

(require "2.07.rkt")

(define x (make-interval 6.12 7.48))

(add-interval x x)
(mul-interval x x)
(div-interval x x)