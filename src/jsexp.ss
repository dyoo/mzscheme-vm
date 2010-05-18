#lang scheme/base
(require scheme/contract)

;; a jsexp is either a ht, a vec, or a datum.
(define-struct ht (name key-values) #:transparent)
(define-struct vec (items) #:transparent)
(define-struct int (v) #:transparent)
(define-struct lit (v) #:transparent)

(define (jsexp? x)
  (or (ht? x)
      (vec? x)
      (int? x)
      (lit? x)))

(define (lit-value? x)
  #t
  #;(or (boolean? x)
        (symbol? x)
        (char? x)
        (string? x)
        (number? x)))



(provide/contract [struct ht ([name symbol?]
                              [key-values (listof (list/c symbol? jsexp?))])]
                  [struct vec ([items (listof jsexp?)])]
		  [struct int ([v number?])]
                  [struct lit ([v lit-value?])]
                  [jsexp? (any/c . -> . boolean?)])