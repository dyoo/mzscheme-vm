#lang racket/base

(require racket/contract)

(provide/contract [get-interaction-bytecode
                   (any/c #:language-module module-path? . -> . bytes?)])

;; Get the compiler to produce bytecode appropriate with interactive REPLs.

(define ns (make-base-empty-namespace))

;; get-interaction-bytecode: (or sexp syntax) -> bytes
(define (get-interaction-bytecode x #:language-module 
                                  (language-module 'racket/base))
  (let ([module-namespace
         (parameterize ([current-namespace ns])
           (dynamic-require language-module #f)
           (module->namespace language-module))])
    (parameterize ([current-namespace module-namespace])
      (serialize-compiled-code
       (compile (namespace-syntax-introduce 
                 (datum->syntax #f (cons '#%top-interaction x))))))))


;; serialize-compiled-code: compiled-code -> bytes
(define (serialize-compiled-code a-compiled-code)
  (let ([op (open-output-bytes)])
    (write a-compiled-code op)
    (get-output-bytes op)))



;;(require compiler/zo-parse)
;;(zo-parse (open-input-bytes (compile-an-interaction 'x 
;;                                  #:language-module "lang/wescheme.rkt")))
;;(zo-parse (open-input-bytes 
;;           (serialize-compiled-code (compile-an-interaction 'x))))