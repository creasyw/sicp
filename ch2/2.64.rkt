#lang racket

(require "binary_tree_set.rkt")
(provide list->tree)

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

;; a)
;; In every level, the helper function "partial-tree" break the list
;; into two (almost) even parts with length "left-size" and
;; "right-size". Specifically for the operations, it first recursively
;; builds the left-tree, and then from the deepest level of left-tree
;; it completes each corresponding right-tree part. The current root
;; will be returned as handler of the entire tree.
;; Sidenote: it is clever to make the partial-tree has two input
;; parameters, so that we can have the left tree first and then the
;; right tree via playing with the index in the original list. Another
;; methods is to use the append and build left- and right-tree at the
;; same time. But that is similar to what is at 2.63. The latter has
;; O(NlgN) complexity compared with O(N) for the former one.
;; b)
;; Because this algo will traverse each node once, the overall
;; complexity is O(N).
