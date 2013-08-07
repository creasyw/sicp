#lang racket

(require "fixed-point.rkt")

;; calculate the golden ratio
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)