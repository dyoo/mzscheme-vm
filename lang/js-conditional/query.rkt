#lang racket/base

(require racket/contract
         racket/runtime-path
         syntax/modresolve)


(provide/contract [query (module-path? . -> . module-path?)])

(define-runtime-path record.rkt "record.rkt")
(define ns (make-base-empty-namespace))

;; query: module-path -> module-path
;; Given a module, see if it's implemented via Javascript.
(define (query a-module-path)
  (let ([resolved-path (resolve-module-path a-module-path #f)])
    (parameterize ([current-namespace ns])
      (dynamic-require a-module-path (void)) ;; get the compile-time code running.
      ((dynamic-require-for-syntax record.rkt 'lookup-javascript-implementation) resolved-path))))
  
