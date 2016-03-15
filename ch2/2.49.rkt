#lang racket

;; import segment->painter
(require "2.48.rkt")


(define (draw-outline frame)
  (letrec ((origin (get-origin frame))
           (edge1 (get-edge1 frame))
           (edge2 (get-edge2 frame))
           (diagonal (add-vect edge1 edge2))
           (s1 (make-segment origin (add-vect origin edge1)))
           (s2 (make-segment origin (add-vect origin edge2)))
           (s3 (make-segment (end-segment s2) (add-vect origin diagonal)))
           (s4 (make-segment (end-segment s1) (add-vect origin diagonal))))
    (segment->painter (list s1 s2 s3 s4))))

;; the rest of the questions are similar. It is all about finding the
;; start point and the end point, and then make segment based on these
;; points, and finally feed the function with a list of segments
