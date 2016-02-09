#lang racket

(define (find-element lst target)
  (cond ((null? lst) #f)
        ((not (pair? lst)) (= lst target))
        (else (or (find-element (car lst) target)
                  (find-element (cdr lst) target)))))

;; test
(displayln (eq? (find-element (list 1 3 (list 5 7) 9) 7) #t))
(displayln (eq? (find-element (list (list 7)) 7) #t))
(displayln (eq? (find-element (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 (list 7))))))) 7) #t))
(displayln (eq? (find-element (list 1 (list 2 (list 3 (list 4 (list 5 (list 6)))))) 7) #f))
