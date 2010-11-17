#lang s-exp "../base.rkt"



(require "private/check-expect.rkt"
         (for-syntax racket/base))
(provide (all-from-out "private/check-expect.rkt"))



(define-syntax (EXAMPLE stx)
  (syntax-case stx ()
    ((_ test expected)
     #'(check-expect test expected))))

(define-syntax example (syntax-local-value #'EXAMPLE))


(define-struct unexpected-no-error (result))

(define (error-matches? exn msg)
  (string=? (exn-message exn) msg))


(define (check-error/thunk error-thunk expected-message)
  (with-handlers 
      ([unexpected-no-error?
        (lambda (une)
          (error 
           'check-error
           (format
            "check-error expected the error ~s, but got ~s instead."
            expected-message
            (unexpected-no-error-result une))))]
       [exn:fail?
        (lambda (exn)
          (unless (error-matches? exn expected-message)
            (error 'check-error
                   (format 
                    "check-error expected the error ~s, but got ~s instead."
                    expected-message
                    (exn-message exn)))))])
    (let ([result (error-thunk)])
      (raise (make-unexpected-no-error result)))))


(define-syntax (check-error stx)
  (syntax-case stx ()
    [(_ expr str)
     (syntax/loc stx
       (check-error/thunk (lambda () expr)
                          str))]))






(provide example EXAMPLE check-error)