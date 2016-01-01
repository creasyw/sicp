#lang racket

(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (segment-start seg)
  (car seg))
(define (segment-end seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define average
  (lambda (x y) (/ (+ x y) 2)))

(define (midpoint-segment seg)
  (let ((start (segment-start seg))
        (end (segment-end seg)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
