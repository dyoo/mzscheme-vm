#lang racket/base

(require '#%paramz)

;; Reuse the same exception-handler-key.
(provide exception-handler-key
         
         parameterization-key)



;; provide-stub-parameter
#;(require (for-syntax racket/base))
#;(define-syntax (provide-stub-parameter stx)
  (syntax-case stx ()
    [(_ name ...)
     (syntax/loc stx
       (begin (begin (define (name . args) 
                       'this-is-a-stub)
                     (provide name))
              ...))]))
#;(provide-stub-parameter exception-handler-key)