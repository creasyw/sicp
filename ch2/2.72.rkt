;; a problem without code =D
;; To encode the most frequent symbol is O(1).
;; To encode the least frequent symbol is O(n^2). Specifically, since
;; thee huffman tree in the setting of 2.71 is "linear" (referring to
;; 2.71), the complexity comes from searching if the symbol is in the
;; set of symbols. For the least frequent symbol, the sum of this kind
;; of operations is 1+2+...+n=O(n^2).
