#lang racket

(define (last-pair lst)
  (if (= (length lst) 1) lst
      (last-pair (cdr lst))))

;; testing
(last-pair (list 23 72 149 34))