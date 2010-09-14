#lang racket/base

;; Special language level where implementation is done in Javascript.

(require (for-syntax racket/base)
         (for-syntax racket/file)
         (for-syntax syntax/modresolve)
         (for-syntax "record.rkt"))


(define-for-syntax (read-implementation a-module-path)
  (let ([a-path (resolve-module-path a-module-path (current-load-relative-directory))])
    (file->string (open-input-file a-path))))
    

(define-syntax (require-js stx)
  (syntax-case stx ()
    [(_ path ...)
     (andmap (compose string? syntax-e) (syntax->list #'(path ...)))
     (with-syntax 
         ([(impl ...) (map (compose read-implementation syntax-e)
                           (syntax->list #'(path ...)))])
       (syntax/loc stx
         (begin
           (begin-for-syntax
             (let* ([this-module (variable-reference->resolved-module-path (#%variable-reference))]
                    [key (resolved-module-path-name this-module)])
               (record-implementations! key (list (#%datum . impl) ...))))
           (void))))]))

                                       
(define-syntax (-provide stx)
  (syntax-case stx ()
    [(_ name ...)
     (andmap (compose symbol? syntax-e) (syntax->list #'(name ...)))
     (syntax/loc stx
       (begin
         (begin-for-syntax
           (let* ([this-module (variable-reference->resolved-module-path (#%variable-reference))]
                    [key (resolved-module-path-name this-module)])
               (record-exports! key (list (#%datum . name) ...))))
         (void)))]))




           
(provide require-js 
	 (rename-out (-provide provide)
                     (#%plain-module-begin #%module-begin)))
