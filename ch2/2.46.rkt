#lang racket

(provide (all-define-out))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v)
                                    (edge1-frame fram))
                        (scale-vect (ycor-vect v)
                                    (edge2-frame frame))))))

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cadr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (scale v s)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect y))))

(define (negate v) (scale v -1))

(define (sub-vect v1 v2)
  (add_vect v1 (negate v2)))
