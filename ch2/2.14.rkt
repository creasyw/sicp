#lang racket

;; all regular interval operations
(require "2.07.rkt")
;; import eq-interval?
(require "2.11.rkt")

(require "2.13.rkt")


(define (div-interval1 m n)
  (if (eq-interval? m n)
      (make-interval 1.0 1.0)
      (div-interval m n)))

(define (par1 r1 r2)
  (div-interval1 (mul-interval r1 r2)
                 (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval1 one
                   (add-interval (div-interval1 one r1)

                                 (div-interval1 one r2)))))

(define x (make-interval 2 8))
(displayln (not (eq-interval? (par1 x x) (par2 x x))))

;; this is because the concept of "interval" does not consider the
;; case that (div-interval x x) is 1.0, but in the previous
;; implementation it would still be an interval

;; use center and percentage as underlying assumption
(define (par3 r1 r2)
  (div-percentage (mul-percentage r1 r2)
                  (add-interval r1 r2)))

(define (par4 r1 r2)
  (reverse-cp (add-interval (reverse-cp r1)
                            (reverse-cp r2))))
