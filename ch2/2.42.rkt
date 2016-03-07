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


(define (enumerate-interval n m)
  (range n (+ m 1)))

(define (empty-board) '())

(define (adjoin-position new-row new-col rest-of-queenss)
  (cons (list new-row new-col) rest-of-queenss))

(define (safe? new-col positions)
  (letrec ((new (first positions))
           (old (drop positions 1)))
    (foldl (lambda (itr acc) (and (not (conflict? new itr)) acc)) #t old)))

(define (conflict? x y)
  (or (= (car x) (car y))
      (= (cadr x) (cadr y))
      ;; the mapping position for x in thr row of y
      (= (+ (cadr x) (- (car y) (car x))) (cadr y))))
