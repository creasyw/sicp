#lang racket

(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position new-row k rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))

;; map of map is essentially a two-layers nested loop. Filter is then
;; applied to the output of the two-layers loop. So, for a specific N,
;; the final result of two versions are the same
;; The problem is two-folds:
;; 1. Put the function call into the inner loop will call the function
;;    (N-1) MORE times than what does in the previous version.
;; 2. Via function expansion, the 1st version will run the board size
;;    1, and then incrementally 2, 3, and etc.. While in the 2nd
;;    version, board size N will **recursively** call board size
;;    smaller than N $N^k$ times where k is the level of the recursive
;;    call. It is similar to the difference between regular recursive
;;    function and tail-recursion
;;
;; For example:
;; Assuming the time complexity of board size n is Tn, and space
;; complexity is P_n.
;;
;; For version 1: T_N = \sum_{i=0}_{N-1} (N \times P_i)
;; For version 2: T'_N = \sum_{i=0}_{N-1} (N^{N-i} \times P_i).
;; To answer the question, T'_N - T_N = (N-1) \times T_{N-1} = O(N^N)
