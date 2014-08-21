;; the implementation refers to 2.83. It includes adding "'drop"
;; procedure in every package, implementing "drop" function within the
;; apply-generic function, and change "apply-to-two" so that the
;; unmatched procedure returns false rather than throwing error message.
