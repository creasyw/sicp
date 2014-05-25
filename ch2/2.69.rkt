#lang racket

(require "huffman-tree.rkt")
(provide generate-huffman-tree)

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (cond ((= 0 (length pairs)) '())
        ((= 1 (length pairs)) (car pairs))
        (#t (successive-merge (adjoin-set (make-code-tree (car pairs)
                                                          (cadr pairs))
                                          (drop pairs 2))))))

;; testing
;(define lst (list '(A 4) '(B 2) '(C 1) '(D 1)))
;(define x (generate-huffman-tree lst))
;(displayln x)
;(displayln (left-branch x))
;(displayln (right-branch x))
