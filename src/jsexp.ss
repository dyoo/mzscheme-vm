#lang scheme/base
(require scheme/contract)

;; a jsexp is either a ht, a vec, or a datum.
(define-struct ht (name key-values) #:transparent)
(define-struct vec (items) #:transparent)


(define (jsexp? x)
  (or (ht? x)
      (vec? x)
      (symbol? x)
      (char? x)
      (string? x)
      (number? x)))



(provide/contract [struct ht ([name symbol?]
                              [key-values (listof jsexp?)])]
                  [struct vec ([items (listof jsexp?)])]
                  [jsexp? (any/c . -> . boolean?)])