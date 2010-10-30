#lang s-exp "kernel.rkt"

(require "kernel.rkt"
         "paramz.rkt"
         (for-syntax racket/base))

(provide (except-out (all-from-out "kernel.rkt") #%app))
(provide with-handlers)




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

