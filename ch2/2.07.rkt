#lang racket

(provide (all-defined-out))

(define (make-interval a b) (cons a b))

;; round the number 'x' after 'precison' decimal places
(define (round-off x precision)
  (letrec ((base (expt 10 precision)))
    (/ (round (* x base)) base)))

(define tolerance 2)

;; 2.07
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define add
  (lambda (x y) (round-off (+ x y) tolerance)))

(define (add-interval x y)
  (make-interval (add (lower-bound x) (lower-bound y))
                 (add (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
;; 2.10
(define (div-interval x y)
  (if (or (= 0 (upper-bound y))
          (= 0 (lower-bound y)))
      (displayln "y might not be divisible")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; 2.08
(define sub
  (lambda (x y) (round-off (- x y) tolerance)))

(define (sub-interval x y)
  (make-interval (sub (upper-bound x) (lower-bound y))
                 (sub (lower-bound x) (upper-bound y))))
