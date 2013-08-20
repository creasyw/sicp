#lang racket

;; import tree-map
(require "2.31.rkt")

(define (square-tree tree) (tree-map (lambda (x) (* x x)) tree))

;; testing
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))