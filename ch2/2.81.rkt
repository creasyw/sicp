;; a.
;; Louis's solution would generate infinite loop if the "'proc" does
;; not define in the given package. On the other hand, if the "'proc"
;; has been defined, the same-type coercion will never be called.
