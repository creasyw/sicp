#lang racket

(require "2.50.rkt")

(define (below painter1 painter2)
  (letrec ((split-point (make-vect 0.0 0.5))
           (paint-up
            (transform-painter painter1
                               split-point
                               (make-vect 1.0 0.0)
                               (make-vect 0.0 1.0)))
           (paint-down
            (transform-painter painter2
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point)))
    (lambda (frame)
      (paint-up frame)
      (paint-down frame))))
