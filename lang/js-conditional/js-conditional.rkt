#lang scheme/base
(require (for-syntax racket/base 
                     racket/file
                     syntax/parse
                     syntax/modresolve
                     "record.rkt"))

(define-for-syntax (read-implementation a-module-path)
  (let ([a-path (parameterize ([current-directory (or (current-load-relative-directory)
                                                      (current-directory))])
                  (resolve-module-path a-module-path #f))])
    (file->string a-path)))


(define-syntax (declare-conditional-implementation stx)
  (syntax-parse stx
    [(_ #:racket racket-module-name
        #:javascript javascript-module-name)
     (with-syntax 
         ([impl (read-implementation (syntax-e #'javascript-module-name))])
       (syntax/loc stx
         (begin
           
           ;; Compile-time code: record the Javascript implementation here.
           (begin-for-syntax
             (let* ([this-module (variable-reference->resolved-module-path (#%variable-reference))]
                    [key (resolved-module-path-name this-module)])
               (record-javascript-implementation! key (#%datum . impl))))

           (require racket-module-name)
           (provide (all-from-out racket-module-name)))))]))


(provide declare-conditional-implementation
         (rename-out [#%plain-module-begin #%module-begin]))