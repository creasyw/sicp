#lang racket


(define (unique-triple n)
  (append-map (lambda (i)
                (append-map (lambda (j)
                              (map (lambda (k) (list k j i))
                                   (range 1 j)))
                            (range 2 i)))
              (range 3 (+ n 1))))


(define (sum-list lst)
  (foldl + 0 lst))


(define (triple-sum n s)
  (filter (lambda (lst) (= (sum-list lst) s))
          (unique-triple n)))
