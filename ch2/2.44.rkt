#lang racket

(define wave2 (beside wave (filp-vert wave)))
(define wave4 (below wave2 wave2))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (filp-vert painter))))
    (below painter2 painter2)))

(define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0) painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0) painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (letrec ((quarter (corner-split painter n))
           (half (beside (flip-horiz quarter) quarter)))
    (below (flip-vert half) half)))

(define (up-split painter n)
  (if (= n 0) painter
      (let (smaller (up-split painter (- n 1)))
        (below (beside smaller smaller) painter))))
