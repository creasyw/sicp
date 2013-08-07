#lang racket

(define (printing guess step)
  (display "Step: ")
  (display step)
  (display "  ")
  (display "Guess: ")
  (displayln guess))

(define (fixed-point f first-guess)
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

(define question
  (lambda (x) (/ (log 1000) (log x))))

;; test
(fixed-point question 2.0)

(define (average-damping f)
  (lambda (x) (/ (+ x (f x)) 2)))

;; find fixed-point with average damping
(displayln "With average damping:")
(fixed-point (average-damping question) 2.0)
