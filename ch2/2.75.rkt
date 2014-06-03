#lang racket

(require (except-in "complex_number.rkt" make-from-real-imag make-from-mag-ang))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (expt x 2) (expt y 2))))
          ((eq? op 'angle) (atan y x))
          (#t (error "Unknow op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)
