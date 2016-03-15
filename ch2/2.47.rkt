#lang racket

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (get-origin1 frame) (first frame))
(define (get-edge11 frame) (second frame))
(define (get-edge12 frame) (last frame))


(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (get-origin2 frame) (car frame))
(define (get-edge21 frame) (car (cdr frame)))
(define (get-edge22 frame) (cadr (cdr frame)))
