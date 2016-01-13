#lang racket

;; import basic operations about make-interval
(require "2.07.rkt")
;; import eq-interval?
(require "2.11.rkt")

(provide (all-defined-out))

(define (make-center-percent c r)
  (make-interval (mult c (- 1 r)) (mult c (+ 1 r))))

;; It is REALLY painful to do floating point calculation...
(define (center i)
  (div (+ (lower-bound i) (upper-bound i)) 2.0))

(define (percent i)
  (letrec ((c (center i)))
    (div (- (upper-bound i) c) c)))


;; test
(define x (make-center-percent 6.8 0.1))
(eq-interval? x (make-interval 6.12 7.48))
(= (center x) 6.8)
(= (percent x) 0.1)
