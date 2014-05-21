#lang racket

(require "binary_tree_set.rkt")

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (letrec ((left-size (quotient (- n 1) 2))
               (left-result (partial-tree elts left-size))
               (left-tree (car left-result))
               (non-left-elts (cdr left-result))
               (right-size (- n (+ left-size 1)))
               (this-entry (car non-left-elts))
               (right-result (partial-tree (cdr non-left-elts) right-size))
               (right-tree (car right-result))
               (remaining-elts (cdr right-result)))
        (cons (make-tree this-entry left-tree right-tree) remaining-elts))))
