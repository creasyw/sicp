#lang racket

;; import accumulate
(require "general_func.rkt")

;; import accumulate-n
(require "2.36.rkt")

(define (dot-product v w)
  (accumulate + 0 (map (lambda (x y) (* x y)) (flatten v) (flatten w))))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product v row)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (col) (matrix-*-vector m col)) cols)))

;; testing
(define x (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(dot-product x x)
(matrix-*-vector x (list 1 1 1 1))
(transpose x)
(matrix-*-matrix x (transpose x))