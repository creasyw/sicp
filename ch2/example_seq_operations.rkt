#lang racket

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        ((else (+ (sum-odd-squares (car tree))
                  (sum-odd-squares (cdr tree)))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n) nil
        (letrec ((f fib k))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

;; there is nothing wrong with the two functions above -- naturally
;; written recursive functions. But it also means that in every node
;; of the tree there is identical operation performs, which include
;; check validity of the operation and the specific operation.

;; "wishful thinking" is using here :)
(define (sum-odd-squares2 tree)
  (foldl +
         0
         (map square
              (filter odd?
                      (enumerate-tree tree)))))

(define (even-fibs2 n)
  (foldr cons
         '()
         (filter even?
                 (map fib
                      (enumerate-interval 0 n)))))

;; the basic difference between these two sets of functions is that
;; the latter set makes the enumeration of the data structure in the
;; innermost place while the former keeps it in the surface. That is
;; also the (only) difference between these two functions.
;;
;; That is, if the "wishful thinking" is used here and the data
;; eventually fed into the program is a list regardless of the
;; original structure, we can use the blocks as if we're processing the
;; series of signals (lists of data)! Refer to Lecture 5 for more info.
