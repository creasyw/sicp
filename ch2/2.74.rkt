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

;; c)
;; Each division needs to implement a predicate
;; in-this-division?.
(define (find-employee-record employee files)
  (cond ((null? files) (error "unknown employee" employee))
        ((in-this-division? employee (division (car files)))
         (get-record employee (car files)))
        (else (find-employee-record
               employee (cdr files)))))

(define (in-this-division? employee division)
  ((get 'in-this-division? division) employee))
