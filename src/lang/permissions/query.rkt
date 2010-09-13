#lang racket/base

(require racket/contract
         racket/runtime-path
         syntax/modresolve)

(provide/contract [query
                   (module-path? . -> . (listof string?))])

(define-runtime-path record.rkt "record.rkt")

(define ns (make-base-empty-namespace))


;; query: module-path -> (listof string)
;; Given a module, see what permissions it is declaring.
(define (query a-module-path)
  (let ([resolved-path (resolve-module-path a-module-path #f)])
    (parameterize ([current-namespace ns])
      (dynamic-require a-module-path (void)) ;; get the compile-time code running.
      (let ([result
             ((dynamic-require-for-syntax record.rkt 'lookup) resolved-path)])
        result))))
  
