#lang racket/base
(require racket/runtime-path
         racket/path)

(provide module-resolver)


(define the-module-name-resolver (current-module-name-resolver))

; This defines a custom module resolver that only knows about 
; certain hardcoded modules; all other module lookup requests are delegated
; off to the real module name resolver.


(define-runtime-path base "../../")

(define overridden-modules
  '(bootstrap/bootstrap-teachpack))


;; overriden-module-path?: module-path -> boolean
(define (overridden-module-path? a-path)
  (and (symbol? a-path)
       (member a-path overridden-modules)))


;; redirect-path: symbol -> path
(define (redirect-path a-path)
  (apply build-path base (regexp-split rx"/" (symbol->string a-path))))


(define module-resolver
  (case-lambda 
    ;; When given one argument, it's the name for a module
    ;; declaration that is already loaded.  No need to try to redirect.
    [(resolved-module-name)
     (the-module-name-resolver resolved-module-name)]
    
    [(module-path source-module should-load?)
     (cond [(symbol? module-path)
            (cond 
              [(overriden-module-path? a-path)
               (the-module-name-resolver (redirect-path module-path)
                                         source-module
                                         should-load?)]
              [else
               (the-module-name-resolver module-path source-module
                                         should-load?)])]
           [else
            (the-module-name-resolver module-path source-module
                                      should-load?)])]

    [(module-path source-module stx should-load?)
     (cond [(symbol? module-path)
            (cond 
              [else
               (the-module-name-resolver module-path source-module stx should-load?)])]
           [else
            (the-module-name-resolver module-path source-module stx should-load?)])]))


