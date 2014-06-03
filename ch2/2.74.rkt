#lang racket

;; a)
;; Each division needs to supply the name of the division
;; and its original file to make a generic file
(define (get-record employee generic-file)
  ((get 'get-record (division generic-file))
   employee (original-file generic-file)))


(define (make-generic-file division file)
  (cons division file))
(define (division generic-file)
  (car generic-file))
(define (original-file generic-file)
  (cdr generic-file))

;; b)
;; The generic record should contain the division name
;; and the original record.
(define (get-salary generic-record)
    ((get 'get-salary (division generic-record))
        (original-record generic-record)))

(define (make-generic-record division record)
    (cons division record))
(define (division generic-record)
    (car generic-record))
(define (original-record generic-record)
    (cdr generic-record))
