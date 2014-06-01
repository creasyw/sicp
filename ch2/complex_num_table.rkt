#lang racket

;; import mlist and related functions
(require compatibility/mlist)
(provide (all-defined-out))

(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((impair (assoc key-1 (mcdr local-table))))
        (if impair
            (letrec ((subtable (mcons (car impair) (cdr impair)))
                     (record (assoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                             (cons (cons key-2 value)
                                   (mcdr subtable)))))
            (set-mcdr! local-table
                       (cons (list key-1
                                   (cons key-2 value))
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
