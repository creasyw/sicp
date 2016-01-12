#lang racket

;; import basic operations about make-interval
(require "2.07.rkt")
;; import eq-interval?
(require "2.11.rkt")

(define (make-center-percent c r)
  (make-interval (* c (- 1 r)) (* c (+ 1 r))))

;; It is REALLY painful to do floating point calculation...
(define (center i)
  (round-off (/ (+ (lower-bound i) (upper-bound i)) 2) tolerance))

(define (percent i)
  (letrec ((c (center i)))
    (round-off (/ (- (upper-bound i) c) c) tolerance)))


;; test
(define x (make-center-percent 6.8 0.1))
(eq-interval? x (make-interval 6.12 7.48))
(= (center x) 6.8)
(= (percent x) 0.1)
