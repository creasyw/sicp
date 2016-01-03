#lang racket

;; assume the pair of numbers are non-negative, so the recursive way
;; of finding car or cdr could work
(define (cons2 a b)
  (lambda (selector)
    (selector (* (expt 2 a) (expt 3 b)))))

(define (car2 z)
  (z (lambda (num)
       (define (helper temp acc)
         (if (> (modulo temp 2) 0) acc
             (helper (/ temp 2) (+ acc 1))))
       (helper num 0))))

(define (cdr2 z)
  (z (lambda (num)
       (define (helper temp acc)
         (if (> (modulo temp 3) 0) acc
             (helper (/ temp 3) (+ acc 1))))
       (helper num 0))))


;; test
(define x (cons2 5 7))
(eq? (car2 x) 5)
(eq? (cdr2 x) 7)
