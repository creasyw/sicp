#lang racket
(provide gen-prime)
(provide prime?)

(define (looping base lst)
  (define (helper count new_lst)
    (let ((index (* base count)))
      (if (> index (length new_lst)) new_lst
          (helper (+ count 1)
                  (append (append (take new_lst index) '(0)) (drop new_lst (+ index 1)))))))
  (helper 1 lst))

;; generate all prime numbers smaller than n
;; n should be positive and larger than one
(define (gen-prime n)
  (define (helper target ref)
    (cond ((null? ref) target)
          ((= 0 (list-ref target (car ref))) (helper target (cdr ref)))
          (#t (helper (map (lambda (index) 
                             (if (and (not (= index (car ref))) (= 0 (modulo index (car ref)))) 0 index)) target) 
                      (cdr ref)))))
  (let ((candidates (for/list ((x (in-range n))) x))
        (prime-range (for/list ((x (in-range 2 (+ 1 (integer-sqrt n))))) x)))
    (drop (filter (lambda (x) (not (= 0 x)))
                  (helper candidates prime-range)) 1)))

;; determine if a number is prime
;; n should be positive and larger than one
(define (prime? n)
  (let ((root (integer-sqrt n)))
    (define (helper i)
      (cond ((> i root) #t)
            ((= 0 (modulo n i)) #f)
            (#t (helper (+ i 1)))))
    (and (> n 1) (helper 2))))