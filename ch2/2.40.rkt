#lang racket


(define (unique-pairs n)
  (append-map (lambda (i)
                (map (lambda (j) (list j i))
                     (range 1 i)))
              (range 2 (+ n 1))))


(define (make-pair-sum lst)
  (list (car lst) (cadr lst) (+ (car lst) (cadr lst))))


(define (prime-sum? p)
  (prime? (+ (car p) (cadr p))))


(define (prime? n)
  (foldl (lambda (i j) (and i j))
         #t
         (map (lambda (i) (not (integer? (/ n i))))
              (range 2 (+ (/ n 2) 1)))))


;; typical example of three-blocks program --
;; generator, processor, and output
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
