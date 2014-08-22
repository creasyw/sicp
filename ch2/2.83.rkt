#lang racket

;; this is a modified version of generic_arithmetic.rkt.
;; there is no explicit coercion. The "raise" and "drop" are
;; implemented instead. Besides, the additional considerations are:
;; 1. decide which data type has relatively higher rank. (2.84)
;; 2. "compatible" with later installed new level. (2.84 and 2.85)
;; 3. dealing with more than two arguments

;; the updated implementation refers to 2.86
