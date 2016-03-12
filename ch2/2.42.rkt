#lang racket

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (append-map (lambda (rest-of-queens)
                              (map (lambda (new-row)
                                     (adjoin-position new-row k rest-of-queens))
                                   (enumerate-rows 1 board-size)))
                            (queen-cols (- k 1))))))
  (queen-cols board-size))


(define (enumerate-rows n m)
  (range n (+ m 1)))

(define (safe? new-col positions)
  ;; empty is always safe to insert new cols
  (or (empty? positions)
      (letrec ((new (first positions))
               (old (drop positions 1)))
        (foldl (lambda (itr acc) (and (not (conflict? new itr)) acc)) #t old))))

(define (conflict? x y)
  (or (= (car x) (car y))
      (= (cadr x) (cadr y))
      ;; "diagonal" means the slope of line from x to y is 1 or -1
      (= 1 (abs (/ (- (car x) (car y)) (- (cadr x) (cadr y)))))))

;; define the basic representation to store queues' positions
;; It could be either a list of pairs, which is implemented here.
;; Or a simple list with assumption that the index is the row and the
;; corresponding value is the column, or vice versa.
(define (adjoin-position new-row new-col rest-of-queens)
  (cons (list new-row new-col) rest-of-queens))
(define empty-board '())
