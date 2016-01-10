#lang racket

(require "2.07.rkt")
0
(define (mul-interval2 x y)
  (cond ((> (upper-bound y) (upper-bound x))
         (mul-interval2 y x))
        ;; there are four cases for multiplication of two upper
        ;; bounds, and then consider lower bounds, the overall cases
        ;; are 2*2 + 2*1 + 2*1 + 1*1 = 9, two of which cases need more
        ;; than two multiplications. The first condition would cut the
        ;; edges half.
        ((and (>= (upper-bound x) 0) (>= (upper-bound y) 0))
         (cond ((and (>= (lower-bound x) 0) (>= (lower-bound y) 0))
                (make-interval (mult (lower-bound x) (lower-bound y))
                               (mult (upper-bound x) (upper-bound y))))
               ((and (>= (lower-bound x) 0) (< (lower-bound y) 0))
                (make-interval (mult (upper-bound x) (lower-bound y))
                               (mult (upper-bound x) (upper-bound y))))
               ((and (< (lower-bound x) 0) (>= (lower-bound y) 0))
                (make-interval (mult (lower-bound x) (upper-bound y))
                               (mult (upper-bound x) (upper-bound y))))
               ((and (< (lower-bound x) 0) (< (lower-bound y) 0))
                (make-interval (min (mult (upper-bound x) (lower-bound y))
                                    (mult (upper-bound y) (lower-bound x)))
                               (max (mult (upper-bound x) (upper-bound y))
                                    (mult (lower-bound x) (lower-bound y)))))))
        ((and (>= (upper-bound x) 0) (< (upper-bound y) 0))
         (cond ((>= (lower-bound x) 0)
                (make-interval (mult (upper-bound x) (lower-bound y))
                               (mult (lower-bound x) (upper-bound y))))
               (else
                (make-interval (mult (upper-bound x) (lower-bound y))
                               (mult (lower-bound x) (lower-bound y))))))
        ((and (< (upper-bound x) 0) (< (upper-bound y) 0))
         (make-interval (mult (upper-bound x) (upper-bound y))
                        (mult (lower-bound x) (lower-bound y))))
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
