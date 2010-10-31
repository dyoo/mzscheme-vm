#lang s-exp "kernel.rkt"

(require "kernel.rkt"
         "paramz.rkt"
         (for-syntax racket/base))

(provide (except-out (all-from-out "kernel.rkt") #%app))
(provide with-handlers time do)




;; application: this version includes the stack trace.
(define-syntax (-#%app stx)
  (syntax-case stx ()
    [(_ operator operands ...)
     (with-syntax ([key 'moby-stack-record-continuation-mark-key]
                   [pos (vector (format "~s" (syntax-source stx))
		                (syntax-position stx)
				(syntax-line stx)
				(syntax-column stx)
				(syntax-span stx))])
     (syntax/loc stx
       (with-continuation-mark 'key 'pos
         (#%app operator operands ...))))]))

(provide (rename-out [-#%app #%app]))



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
		(begin body ...))))
          prompt-tag
          (lambda (exn)
            (cond
              [(test? exn)
               (exn-handler exn)]
              ...
              [else
               (raise exn)])))))]))



(define-syntax (time stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax/loc stx
       (let* ([start-time (current-inexact-milliseconds)]
              [val expr]
              [end-time (current-inexact-milliseconds)])
         (printf "time: ~s\n" (- end-time start-time))
         val))]))


(define-syntax (do stx)
  (syntax-case stx ()
    [(_ ([id init-expr step-expr-maybe] ...)
        (stop?-expr finish-expr ...)
        body ...)
     (syntax/loc stx
       (let* ([id init-expr] ...)
         (let loop ([id id] ...)
           (cond [stop?-expr
                  body ...
                  (loop step-expr-maybe ...)]
                 [else
                  finish-expr ...]))))]
    
    [(_ ([id init-expr step-expr-maybe] ...)
        (stop?-expr)
        body ...)
     (syntax/loc stx
       (do ([id init-expr step-expr-maybe] ...)
         (stop?-expr (void))
         body ...))]))

