#lang s-exp "base.rkt"
(require "wescheme.rkt")
(require (for-syntax racket/base))

(provide (except-out (all-from-out "wescheme.rkt")
		     require))


(define-syntax (restricted-require stx)
  (syntax-case stx ()
    [_
     (raise-syntax-error #f "currently disallowed in interactions" stx)]))

(provide (rename-out [restricted-require require]))