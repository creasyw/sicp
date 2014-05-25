#lang racket

(require "huffman-tree.rkt")
;; import encode
(require "2.68.rkt")
;; import generate-huffman-tree
(require "2.69.rkt")

(define lyric (list '(GET A JOB)
                    '(SHA NA NA NA NA NA NA NA NA)
                    '(GET A JOB)
                    '(SHA NA NA NA NA NA NA NA NA)
                    '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP)
                    '(SHA BOOM)))
(define pairs (list '(A 2) '(BOOM 1) '(GET 2) '(JOB 2)
                    '(NA 16) '(SHA 3) '(YIP 9) '(WAH 1)))

(define (encode-song lyrics tree)
  (if (null? lyrics)
      '()
      (append (encode (car lyrics) tree)
              (encode-song (cdr lyrics) tree))))

(define (fix-length lyrics num-symbols)
  (letrec ((word-length (/ (log num-symbols) (log 2))))
    (* (length (flatten lyrics)) word-length)))

(define q2-70
  (letrec ((tree (generate-huffman-tree pairs))
           (coded-words (encode-song lyric tree)))
    (printf "The coded words are: ~a\n" coded-words)
    (printf "Its length is: ~a\n" (length coded-words))
    (printf "Without variable length coding, the length will be: ~a\n"
               (fix-length lyric (length pairs)))))           
