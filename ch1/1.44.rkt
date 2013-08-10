#lang racket

(define (repeated f n)
  (lambda (x)
    (define (helper count)
      (if (= count n) (f x)
          (f (helper (+ count 1)))))
     (helper 1)))

(define (smooth f)
  (letrec ((dx 0.00001))
    (lambda (x) (/ (+ (f (+ x dx)) (f x) (f (- x dx))) 3))))

(define (n-fold-smooth f n)
  (let ((n-times (repeated smooth n)))
    (n-times f)))