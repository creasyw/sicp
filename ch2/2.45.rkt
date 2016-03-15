#lang racket


(define (square-of-four tl tr bl br)
  (lambda (painter)
    (letrec ((top (beside (tl painter) (tr painter)))
             (bottom (beside (bl painter) (br painter))))
      (below bottom top))))


(define (flipped-pairs painter)
  (letrec ((combine4 (square-of-four identity flip-vert identity flip-vert)))
    (combine4 painter)))


(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotation180 flip-vert)))
    (combine4 (corner-split painter n))))


(define (split first second)
  (lambda (painter n)
    (if (= n 0) painter
        (letrec ((smaller ((split first second) painter (- n 1))))
          (first painter (second smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))
