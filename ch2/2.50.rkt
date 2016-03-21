#lang racket
(provie (all-defined-out))

;; As described during define the function, applying frame-coord-map
;; to a frame returns a procedure that, given a vector, returns a
;; vector. If the argument vector is in the unit square, the result
;; vector will be in the frame.
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v)
                                    (edge1-frame frame))
                        (scale-vect (ycor-vect v)
                                    (edge2-frame frame))))))
;; This function is also nice by separating a frame from
;; coordinates. A frame is actually only defined by three points, one
;; of which is "origin", and the rest are edge1 and edge2.

;; It is a good example of designing system by dividing it into
;; modules, and then for each module, implement it in a top-down
;; fashion. The former approach defines the API among modules, while
;; the latter one postpone the detailed implementation of the data
;; structure and keep it in a abstract manner.
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (letrec ((m (frame-coord-map frame))
             (new-origin (m origin)))
      (painter (make-frame new-origin
                           (sub-vect (m corner1) new-origin)
                           (sub-vect (m corner2) new-origin))))))


(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))


(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))


(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))


;; Take two procedures of painter(s), combine and produce a new one,
;; which is also take frame as parameter
;; so powerful...
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
                (paint-right frame)))))


(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
