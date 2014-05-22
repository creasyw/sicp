#lang racket

(require "binary_tree_set.rkt")

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        ((< given-key (key (car set-of-records)))
         (lookup given-key (left-tree set-of-records)))
        (#t (lookup given-key (left-tree set-of-records)))))
