#lang racket

(define (pascal-triangle level)
  (define (calc lst)
    (append (list 1)
            (for/list ((i (in-range (- (length lst) 1))))
              (+ (list-ref lst i) (list-ref lst (+ i 1))))
            (list 1)))
  (define (helper upper-level acc)
    (let ((current (calc upper-level)))
      (displayln current)
      (if (> acc level) '()
          (helper current (+ acc 1)))))
  (helper (list 1) 1))