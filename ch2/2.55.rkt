;; The following two command are equivalent:
;;     racket@> 'a
;;     'a
;;     racket@> (quote a)
;;     'a
;;
;; The ''abcd equals to (quote (quote abcd)).
;; As a result, the (car ''abcd) is quote, and (cdr ''abcd) is 'abcd
