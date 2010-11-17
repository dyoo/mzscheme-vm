#lang s-exp "../base.rkt"

(require (for-syntax racket/base)
         "../location.rkt")

(provide check-expect 
         check-within 
         check-error
         run-tests)


(define-for-syntax (syntax-location-values stx)
  (list (format "~a" (syntax-source stx))
        (syntax-position stx)
        (syntax-line stx)
        (syntax-column stx)
        (syntax-span stx)))


(define-syntax (check-expect stx)
  (syntax-case stx ()
    [(_ test expected)
     (with-syntax ([(id offset line column span)
                    (syntax-location-values stx)])
       #'(check-expect* (make-location id offset line column span)
                        (lambda () test)
                        (lambda () expected)))]))
    
(define-syntax (check-within stx)
  (syntax-case stx ()
    [(_ test expected delta)
     #'(void)]))


(define-syntax (check-error stx)
  (syntax-case stx ()
    [(_ test expected-msg)
     #'(void)]))



(define (run-tests)
  (void))





#|
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



|#
