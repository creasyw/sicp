;; Given the coercion procedure provided in the question, if there is
;; valid mix-procedure operations based on type1 and type2, but the
;; apply-generic takes arguments type2 and type3, the program will
;; only search operations within type2 and type3 and neglect whether
;; type3 could convert to type1. Hence, this strategy is insufficient.
