#lang racket

(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (segment-start seg)
  (car seg))
(define (segment-end seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))
(define (x-coordinate p)
  (car p))
(define (y-coordinate p)
  (cdr p))

(define average
  (lambda (x y) (/ (+ x y) 2)))

(define (midpoint-segment seg)
  (let ((start (segment-start seg))
        (end (segment-end seg)))
    (make-point (average (x-coordinate start) (x-coordinate end))
                (average (y-coordinate start) (y-coordinate end)))))

;; constructor -- use two points to define a rectangle
(define make-rectangle cons)
;; selector
(define (get-vertex1 rectangle) (car rectangle))
(define (get-vertex2 rectangle) (cdr rectangle))
(define (get-width rectangle)
  (abs (- (x-coordinate (get-vertex1 rectangle))
          (x-coordinate (get-vertex2 rectangle)))))
(define (get-height rectangle)
  (abs (- (y-coordinate (get-vertex1 rectangle))
          (y-coordinate (get-vertex2 rectangle)))))

(define (get-preimeter rectangle)
  (+ (* 2 (get-width rectangle))
     (* 2 (get-height rectangle))))

(define (get-area rectangle)
  (* (get-width rectangle) (get-height rectangle)))

;; test
(define a (make-point 1 2))
(define b (make-point 5 5))
(define rec (make-rectangle a b))

(eq? (get-preimeter rec) 14)
(eq? (get-area rec) 12)
