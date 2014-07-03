#lang racket

;; import mlist and related functions
(require compatibility/mlist)
(provide (all-defined-out))

;; a nice way to encapsulate functions: the chain of reaction is from
;; function "get" --> tag "'lookup-proc" --> local function "lookup".
;; On the other hand, operation-table --> make-table --> dispatch is
;; used to encapsulate the dispatch function.

(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (letrec ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                             (mcons (mcons key-2 value)
                                   (mcdr subtable)))))
            (set-mcdr! local-table
                       (mcons (mlist key-1
                                   (mcons key-2 value))
                             (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
