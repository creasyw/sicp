#lang racket

(provide fixed-point)
(provide fixed-point-print)
(provide newtons-method)

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next) next
          (try next))))
  (try first-guess))

;; the same functionality as fixed-point
;; but adding print info at each iteration
(define (fixed-point-print f first-guess)
  (define (printing guess step)
    (display "Step: ")
    (display step)
    (display "  ")
    (display "Guess: ")
    (displayln guess))
  (letrec ((tolerance 0.00001))
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) tolerance))
    (define (try guess step)
      (printing guess step)
      (let ((next (f guess)))
        (if (close-enough? guess next)
            (begin (printing guess step) next)
            (try next (+ 1 step)))))
    (try first-guess 1)))

(define (deriv g)
  (letrec ((dx 0.00001))
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))))

(define (newtons-method g guess)
  (define (newton-transform)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))
  (fixed-point (newton-transform) guess))