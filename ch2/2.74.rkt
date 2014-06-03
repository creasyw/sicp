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
