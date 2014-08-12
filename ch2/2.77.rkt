#lang racket

(require "generic_arithmetic.rkt")
(install-complex-package)

(apply-generic 'magnitude '(complex rectangular 3 . 4))
;; 5

;; The four lines of code given by A.P. Hacker build another table
;; which has the real-part/imag-part/magnitude/angle in one axis and
;; complex in the other axis. THis table will strip the first tag of
;; the compound data. The second tag will determine which part of code
;; will be excuted in the function of magnitude. Refer to
;; generic_arithmetic.rkt for more details.
