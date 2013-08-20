#lang racket

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (left-branch m) (car m))
(define (right-branch m) (cadr m))
(define (branch-length b) (car b))
(define (branch-structure b) (cadr b))

(define (weight? branch) (number? (branch-structure branch)))
(define (branch-weight b)
  (if (weight? b) (branch-structure b) (total-weight (branch-structure b))))
(define (total-weight m)
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))

(define (torque b)
  (* (branch-length b) (branch-weight b)))
(define (balanced? m)
  (letrec ((lb (left-branch m))
           (rb (right-branch m)))
    (and
     (= (torque lb) (torque rb))
     (or (weight? lb) (balanced? lb))
     (or (weight? rb) (balanced? rb)))))