#lang racket

;; import repeated
(require "../procedure.rkt")

(define (square x) (* x x))
((repeated square 2) 5)