#lang racket

(define (my-reverse lst)
  (define (helper result acc)
    (if (null? acc) result
        (helper (cons (car acc) result) (cdr acc))))
  (helper '() lst))

;; testing
(define test (list 1 4 9 16 25))
(displayln test)
(reverse test)
(my-reverse test)