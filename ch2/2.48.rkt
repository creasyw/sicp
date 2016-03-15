#lang racket

(provide (all-define-out))

;; import the abstractions of vectors and their basic operations
(require "2.46.rkt")


(define (segment->painter segment-list)
  (lambda (frame)
    (for-each (lambda (segment)
                (draw-line ((frame-coord-map frame) (start-segemnt segment))
                           ((frame-coord-map frame) (end-segment segment))))
              segment-list)))

(define (make-segment v1 v2) (cons v1 v2))

(define (start-segment segment) (car segment))
(define (end-segment segment) (cadr segment))
