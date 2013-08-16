#lang racket

(define (fringe lst)
  (flatten lst))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))
