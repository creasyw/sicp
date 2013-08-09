#lang racket

(provide cont-frac)

;; di and ni should be procedure for infinite calls
;; generating d and n according to certain rules
(define (cont-frac di ni term)
  (define (helper count)
    (if (= count term) (/ (ni count) (di count))
        (/ (ni count) (+ (di count) (helper (+ count 1))))))
  (helper 1))

;; test
;(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100)

(define (precision-control digits f)
  (letrec ((precision (/ 1.0 (expt 10 digits))))
    (define (helper previous current round)
      (if (< (abs (- previous current)) precision) current
          (helper current (f round) (+ round 1))))
    (helper 0 100 1)))

(define (golden-ratio round)
  (/ 1.0 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) round)))

;; test
;(precision-control 4 golden-ratio)