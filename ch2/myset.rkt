#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x myset)
  (cond ((null? myset) false)
        ((= x (entry myset)) true)
        ((< x (entry myset))
         (element-of-set? x (left-branch myset)))
        ((> x (entry myset))
         (element-of-set? x (right-branch myset)))))

(define (adjoin-set x myset)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry myset)) myset)
        ((< x (entry myset))
         (make-tree (entry myset)
                    (adjoin-set x (left-branch myset))
                    (right-branch myset)))
        ((> x (entry myset))
         (make-tree (entry myset)
                    (left-branch myset)
                    (adjoin-set x (right-branch myset))))))