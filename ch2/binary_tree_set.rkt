#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x s1)
  (cond ((null? s1) false)
        ((= x (entry s1)) true)
        ((< x (entry s1))
         (element-of-set? x (left-branch s1)))
        (#t (element-of-set? x (right-branch s1)))))