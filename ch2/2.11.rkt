#lang racket

(require "2.07.rkt")

(define (mul-interval2 x y)
  (cond ((> (upper-bound y) (upper-bound x))
         (mul-interval2 y x))
        ;; In addition to the condition above, there are three
        ;; premises below, upper is larger than lower for each
        ;; interval, and (upper x) larger than (upper y). The left
        ;; independent variable is between (lower x) and (upper y).
        ((and (> (lower-bound x) 0) (> (lower-bound y) 0))
         (make-interval (mult (lower-bound x) (lower-bound y))
                        (mult (upper-bound x) (upper-bound y))))
        ((and (or (> 0 (lower-bound x)) (> 0 (lower-bound y)))
              (> (upper-bound x) 0))
         (make-interval (mult (upper-bound x) (min (lower-bound x) (lower-bound y)))
                        (mult (upper-bound x) (max (lower-bound x) (upper-bound y)))))
        ((> 0 (upper-bound x))
         (make-interval (mult (upper-bound x) (max (lower-bound x) (upper-bound y)))
                        (mult (min (lower-bound x) (upper-bound y)) (lower-bound y))))
        (else (displayln "Should not be here!!!"))))

(define eq-interval?
  (lambda (x y)
    (and (= (lower-bound x) (lower-bound y))
         (= (upper-bound x) (upper-bound y)))))

;; test
(eq-interval? (mul-interval (make-interval -1 10)
                            (make-interval 3 5))
              (mul-interval2 (make-interval -1 10)
                             (make-interval 3 5)))

(eq-interval? (mul-interval (make-interval 1 10)
                            (make-interval -3 5))
              (mul-interval2 (make-interval 1 10)
                             (make-interval -3 5)))

(eq-interval? (mul-interval (make-interval 1 10)
                            (make-interval 3 5))
              (mul-interval2 (make-interval 1 10)
                             (make-interval 3 5)))
