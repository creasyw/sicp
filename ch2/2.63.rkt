#lang racket

;; import all functions defining tree structure
(require "binary_tree_set.rkt")

(provide tree->list)

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

;; (define (tree->list-2 tree)
(define (tree->list tree)
  (define (copy-to-list tree result)
    (if (null? tree)
        result
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result)))))
  (copy-to-list tree '()))

;; a) two functions are the same for every tree
;; b) For the 1st implementation: O(logN)*O(N/2)  (assuming the tree is balanced)
;;    Its first part is the depth of the tree and the second part is for "append" in every level
;;    Hence, the overall complexity is O(NlogN) (it could also written as T(n)=2T(n/2)+O(n/2).)
;;    For the 2nd implementation, every node evokes once with O(1) complexity for "cons",
;;    Hence its overall complexity is O(N).


;; The implementation of "append" in source code
;; Basically, it iterates l1 and make the l2 as the cdr of the last item in l1
;;
;; from racket-master/racket/src/racket/src/list.c
;Scheme_Object *
;scheme_append(Scheme_Object *l1, Scheme_Object *l2)
;{
;  Scheme_Object *first, *last, *orig1, *v;
;
;  orig1 = l1;
;
;  first = last = NULL;
;  while (SCHEME_PAIRP(l1)) {
;    v = cons(SCHEME_CAR(l1), scheme_null);
;    if (!first)
;      first = v;
;    else
;      SCHEME_CDR(last) = v;
;    last = v;
;    l1 = SCHEME_CDR(l1);
;
;    SCHEME_USE_FUEL(1);
;  }
;
;  if (!SCHEME_NULLP(l1))
;    scheme_wrong_contract("append", "list?", -1, 0, &orig1);
;
;  if (!last)
;    return l2;
;
;  SCHEME_CDR(last) = l2;
;
;  return first;
;}
