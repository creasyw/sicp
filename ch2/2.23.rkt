#lang racket

(define (for-each1 proc lst)
  (cond ((null? lst) #t)
        (#t
         (proc (car lst))
         (for-each1 proc (cdr lst)))))
