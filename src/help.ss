#lang scheme/base

(require compiler/zo-parse
         scheme/pretty)

(provide bytecode)



(define (bytecode top-level-form)
  (parameterize ([current-namespace (make-base-namespace)])
    (let-values ([(in out) (make-pipe)])
      (write (compile top-level-form) out)
      (close-output-port out)
      (begin0
        (pretty-display (zo-parse in))
        (close-input-port in)))))