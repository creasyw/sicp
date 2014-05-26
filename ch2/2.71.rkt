#lang racket

;; import basic defs
(require "huffman-tree.rkt")
;; import generate-huffman-tree)
(require "2.69.rkt")

(define (print-tree tree)
  (define (printing prefix tree)
    (printf "~a Symbols:~a, Weight:~a\n."
            prefix
            (symbols tree)
            (weight tree)))
  (define (print-subtree prefix subtree)
    (cond ((null? subtree) '())
          ((leaf? subtree) (printing prefix subtree))
          (#t (printing prefix subtree)
              (print-subtree (string-append prefix "--") (left-branch subtree))
              (print-subtree (string-append prefix "--") (right-branch subtree)))))

  (print-subtree "--" tree))

(print-tree (generate-huffman-tree
             '((a 1) (b 2) (c 4) (d 8) (e 16))))

(printf "\n\n")

(print-tree
 (generate-huffman-tree
  '((a 1) (b 2) (c 4) (d 8 ) (e 16)
    (f 32) (g 64) (h 128) (i 256) (j 512))))

;; because the summation of all n-1 items is equal to the weights of
;; the nth item, the tree will be "linear" -- in the sense that every
;; level will have a leaf node and a subtree contains all items with
;; smaller weights. As a result, the 1st node has length 1, and the
;; last node has n-1 length ocde.
