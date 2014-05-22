#lang racket

;; import basic tree definitions
(require "binary_tree_set.rkt")
;; import intersection-set
(require (only-in "2.61.rkt" intersection-set))
;; import union-set with O(N) for ordered list
(require "2.62.rkt")
;; import tree->list
(require "2.63.rkt")
;; import list->tree
(require "2.64.rkt")

;; both tree->list and list->tree has O(N) complexity, and the
;; union-set for ordered lists is also O(N). On the other hand, if we
;; only convert one of the sets into list and use adjoin-set to insert
;; every item into the latter set, then the first half has O(N) and
;; the 2nd half is O(logN) for each item. As a result, the overall
;; complexity would be O(NlogN)
(define (union-set-tree s1 s2)
  (letrec ((lst1 (tree->list s1))
           (lst2 (tree->list s2)))
    (list->tree (union-set lst1 lst2))))

(define (intersection-set-tree s1 s2)
  (letrec ((lst1 (tree->list s1))
           (lst2 (tree->list s2)))
    (list->tree (intersection-set s1 s2))))
