#lang s-exp "../lang/base.rkt"

(require (for-syntax racket/base)
         (for-syntax "record.rkt"))

(define-syntax (require-permission stx)
  (syntax-case stx ()
    [(_ perms ...)
     (andmap (compose string? syntax-e) (syntax->list #'(perms ...)))
     (with-syntax ([permissions (datum->syntax stx 'permissions)]
                   [src (syntax-source stx)])
       (syntax/loc stx
         (begin
           (begin-for-syntax
             (let* ([this-module (variable-reference->resolved-module-path (#%variable-reference))]
                    [key (resolved-module-path-name this-module)])
               (record! key (list (#%datum . perms) ...))))
           (void))))]))




(provide require-permission)