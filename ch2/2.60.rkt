#lang racket

(require (except-in  "2.59.rkt" adjoin-set))

(define (build-set lst)
  (if (<= (length lst) 1)
      lst
      ;; use "larger than" here for increasing order output
      (letrec ((sorted_lst (sort lst >)))
        (define (helper rest acc)
          (cond ((null? rest) acc)
                ((equal? (car rest) (first acc))
                 (helper (cdr rest) acc))
                (#t (helper (cdr rest) (cons (car rest) acc)))))
        (helper (cdr sorted_lst) (take sorted_lst 1)))))

;; test build-set
;(build-set (list 1 2 3 4 5 6 67 4 5 2 9))

(define (adjoin-set x s1)
  (cons x s1))