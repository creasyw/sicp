#lang racket

(define (use kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (count-change n)
  (define (helper amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (#t (+ (helper amount (- kinds-of-coins 1))
                 (helper (- amount (use kinds-of-coins))
                               kinds-of-coins)))))
  (helper n 5))