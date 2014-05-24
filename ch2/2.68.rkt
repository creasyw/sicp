#lang racket

(require "huffman-tree.rkt")

;; example tree decoding/encoding tree
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (helper acc subtree)
    (cond ((leaf? subtree) (reverse acc))
          ((member symbol (symbols (left-branch subtree)))
           (helper (cons 0 acc) (left-branch subtree)))
          ((member symbol (symbols (right-branch subtree)))
           (helper (cons 1 acc) (right-branch subtree)))
          (#t (error "The symbols in the tree is not consistant -- ENCODE-SYMBOL"))))
  (if (not (member symbol (symbols tree)))
      (error "The symbol:" symbol " cannot be encoded -- ENCODE-SYMBOL")
      (helper '() tree)))

(encode '(A D A B B C A) sample-tree)
;; the result is '(0 1 1 0 0 1 0 1 0 1 1 1 0)
