#lang racket

(define (subsets s)
  (if (null? s) (list null)
      (let ((rest (subsets (cdr s))))
        ;; the "display" will make the procedure clear =D
        (displayln rest)
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;; testing
(subsets (list 1 2 3))