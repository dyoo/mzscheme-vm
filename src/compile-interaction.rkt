#lang racket/base

;; Experiments: trying to figure out how to get the compiler to produce
;; bytecode appropriate with interactive REPLs.

(define ns (module->namespace 'racket/base))

;; compile-an-interaction: any -> compiled-code
(define (compile-an-interaction x)
  (parameterize ([current-namespace ns])
    (compile (namespace-syntax-introduce 
              (datum->syntax #f (cons '#%top-interaction x))))))


(define (serialize-compiled-code a-compiled-code)
  (let ([op (open-output-bytes)])
    (write a-compiled-code op)
    (get-output-bytes op)))
  

(require compiler/zo-parse)

(zo-parse (open-input-bytes (serialize-compiled-code (compile-an-interaction 'x))))