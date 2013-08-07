#lang racket

;; the detail of Simpson's rule refers
;; http://en.wikipedia.org/wiki/Simpson's_rule#Composite_Simpson.27s_rule
;; the value of resolution should be even
(define (integral f a b resolution)
  (letrec ((step (/ (- b a) resolution)))
    (define (summation acc index)
      (displayln acc)
      (cond ((= index resolution) (f b))
            ((= index 0) (+ (f acc) (summation (+ acc step) (+ index 1))))
            ((even? index) (+ (* 2.0 (f acc))
                              (summation (+ acc step) (+ index 1))))
            (#t (+ (* 4.0 (f acc)) (summation (+ acc step) (+ index 1))))))
    (* (/ (- b a) resolution 3) (summation a 0))))