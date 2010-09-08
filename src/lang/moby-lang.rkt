#lang s-exp "kernel.rkt"

(require "kernel.rkt"
         "paramz.rkt"
         (for-syntax racket/base))

(provide (all-from-out "kernel.rkt"))
(provide with-handlers)


(define-syntax (with-handlers stx)
  (syntax-case stx ()
    [(_ ([test? exn-handler] ...) body ...)
     (syntax/loc stx
       (let ([prompt-tag (make-continuation-prompt-tag)])
         (call-with-continuation-prompt 
          (lambda ()
            (begin
              (with-continuation-mark exception-handler-key
                (lambda (exn)
                  (abort-current-continuation prompt-tag exn))
                body ...)))
          prompt-tag
          (lambda (exn)
            (cond
              [(test? exn)
               (exn-handler exn)]
              ...
              [else
               (raise exn)])))))]))
