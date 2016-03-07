#lang racket

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (position) (safe? k position))
                (append-map (lambda (rest-of-queenss)
                              (map (lambda (new-row)
                                     (adjoin-position new-row k rest-of-queenss))
                                   (enumerate-interval 1 board-size)))
                            (queen-cols (- k 1))))))
  (queen-cols board-size))


(define (enumerate-interval 1 n)
  (range 1 (+ n 1)))

(define (empty-board) '())
