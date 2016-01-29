#lang racket

(provide (all-defined-out))

;; Basic Representations
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

;; round the number 'x' after 'precison' decimal places
;; confine the precision to be 0.01
(define tolerance 2)
(define (round-off x precision)
  (letrec ((base (expt 10 precision)))
    (/ (round (* x base)) base)))


(define eq-interval?
  (lambda (x y)
    (and (= (lower-bound x) (lower-bound y))
         (= (upper-bound x) (upper-bound y)))))


;; Operations
(define add
  (lambda (x y) (round-off (+ x y) tolerance)))

(define (add-interval x y)
  (make-interval (add (lower-bound x) (lower-bound y))
                 (add (upper-bound x) (upper-bound y))))

(define mult
  (lambda (x y) (round-off (* x y) tolerance)))

(define (mul-interval x y)
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

(define div
  (lambda (x y) (round-off (/ x y) tolerance)))

(define (div-interval x y)
  (if (or (= 0 (upper-bound y))
          (= 0 (lower-bound y)))
      (displayln "y might not be divisible")
      (mul-interval x
                    (make-interval (div 1.0 (upper-bound y))
                                   (div 1.0 (lower-bound y))))))

(define sub
  (lambda (x y) (round-off (- x y) tolerance)))

(define (sub-interval x y)
  (make-interval (sub (upper-bound x) (lower-bound y))
                 (sub (lower-bound x) (upper-bound y))))


;; Center/Ratio representation
(define (make-center-percent c r)
  (make-interval (mult c (- 1 r)) (mult c (+ 1 r))))

(define (center i)
  (div (+ (lower-bound i) (upper-bound i)) 2.0))

(define (percent i)
  (letrec ((c (center i)))
    (div (- (upper-bound i) c) c)))

