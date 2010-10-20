#lang scheme/base
(require (for-syntax racket/base 
                     syntax/parse
                     "record.rkt"))

(define-syntax (declare-conditional-implementation stx)
  (syntax-parse stx
    [(_ #:racket racket-module-name
        #:javascript javascript-module-name)
     (syntax/loc stx
       (begin
         (begin-for-syntax
           (let* ([this-module (variable-reference->resolved-module-path (#%variable-reference))]
                  [key (resolved-module-path-name this-module)])
             ;; Fill me in: do some compile-time stuff here to record
             ;; this module's javascript implementation.
             (record-javascript-implementation! key (list (#%datum . javascript-module-name)))))
         
         (require racket-module-name)
         (provide (all-from-out racket-module-name))))]))


(provide declare-conditional-implementation
         (rename-out [#%plain-module-begin #%module-begin]))