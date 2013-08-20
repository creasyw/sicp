#lang racket

(provide tree-map)

(define (tree-map proc tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (proc tree))
        (#t (cons (tree-map proc (car tree))
                  (tree-map proc (cdr tree))))))