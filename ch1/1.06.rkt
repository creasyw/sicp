#lang racket

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; The following function will not work properly as "my-sqrt"
;; because as "applicative-order evaluation", "then-clause" and "else-clause"
;; both have to be evaluated before applied.
;; -- this will cause the sqrt-iter infinite recursion
;; without tail-recursive optimization
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x) guess
          (sqrt-iter (improve guess x) x)))