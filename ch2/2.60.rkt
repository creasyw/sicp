#lang racket

(require (except-in  "2.59.rkt" adjoin-set))
(provide build-set)

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

;; Specifically for this question, without duplication, the complexities are
;; O(n) for predicate, O(n) for adjoin, O(n^2) for union and intersection.
;; If the duplicated items exit in the set, the corresponding complexities are
;; O(n) for predicate, O(1) for adjoin, O(n^2) for union and intersection.
;; However, if the N is large and there are many duplicated items in it, this
;; implmentation would dramatically increase the space complexity and
;; deteriorate the performance. (e.g. 90% duplicated, expected time 100x ).
;; Meanwhile, it also complicate the implementations of union and intersection.

;; Hence, it's better to add preprocessing and keep most of funcs as they are
;; in the question 2.59. The most effective and time-saving way is:
;; (define (build-set lst)
;;   (set->list (list->set lst)))
;; with O(NlogN) to build the set from list,
;; though it seems "red herring" as using built-in set to implement set...
;; The implemented method use O(NlogN) sorting and O(N) to eliminate duplicated
;; items. So, the overall complexity for build-set is O(NlogN).
;; The ordered list is beneficial in three folds:
;; 1) The search in any element within the list is O(lgN)
;; 2) The upper- and lower- bounds of the list are helpful to exclude
;;    items out of the range at the first place
;; 3) Generate the set from list takes O(NlogN), while maintaining it
;;    only takes linear time, based on the assumption that the list is
;;    already in order. That is the case for both intersection and union
