#lang racket

(require (except-in  "2.59.rkt" adjoin-set))

(define (build-set lst)
  (set->list (list->set lst)))

(define (adjoin-set x s1)
  (cons x s1))