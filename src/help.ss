#lang scheme/base

(require compiler/zo-parse
         scheme/pretty)

(provide bytecode)



(define (bytecode top-level-form)
  (parameterize ([current-namespace (make-base-namespace)])
    (let-values ([(in out) (make-pipe)])
      (write (compile top-level-form) out)
      (close-output-port out)
      (let ([output (zo-parse in)])
        (begin
          (pretty-display output)
          (close-input-port in)
          output)))))