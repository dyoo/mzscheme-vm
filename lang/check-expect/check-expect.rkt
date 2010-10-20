#lang s-exp "../kernel.rkt"



(require "private/check-expect.rkt")
(provide (all-from-out "private/check-expect.rkt"))




(require (for-syntax racket/base))

(define-syntax (EXAMPLE stx)
  (syntax-case stx ()
    ((_ test expected)
     #'(check-expect test expected))))

(define-syntax example (syntax-local-value #'EXAMPLE))


(provide example EXAMPLE)