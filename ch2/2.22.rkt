#lang racket

(define square
  (lambda (x) (* x x)))

;; 1) this function is to append the newly calculated square at the
;; beginning of the list of answers
(define (square-list-wrong items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

;; 2) reversing the two operands at `cons' would just make things
;; worse -- the resulting answer is a pair, which `car' is the pair of
;; the previous step.

(define (square-list lst)
  (define (helper leftover acc)
    (if (null? leftover) acc
        (helper (cdr leftover)
                (cons (square (car leftover))
                      acc))))
  (reverse (helper lst '())))

;; test
(displayln (square-list (list 1 2 3 4 5)))
