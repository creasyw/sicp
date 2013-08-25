#lang racket

;; import accumulate
(require "general_func.rkt")

(accumulate / 1 (list 1 2 3))
(foldl / 1 (list 1 2 3))

(accumulate list '() (list 1 2 3))
(foldl list '() (list 1 2 3))