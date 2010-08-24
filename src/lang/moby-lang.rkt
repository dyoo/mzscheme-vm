#lang racket/base

(require (for-syntax racket/base))

;; Special forms
(define-syntax (-#%module-begin stx)
  (syntax-case stx ()
    [(_ body ...)
     (syntax/loc stx 
       (#%plain-module-begin body ...))]))

;; datums
(define-syntax (-#%datum stx)
  (syntax-case stx ()
    [(_ . x)
     (syntax/loc stx
       (#%datum . x))]))

;; application
(define-syntax (-#%app stx)
  (syntax-case stx ()
    [(_ operator operands ...)
     (syntax/loc stx
       (#%app operator operands ...))]))


(define-syntax (-define stx)
  (syntax-case stx ()
    [(_ x ...)
     (syntax/loc stx
       (define x ...))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive function stubs

;; provide-stub-function
(define-syntax (provide-stub-function stx)
  (syntax-case stx ()
    [(_ name)
     (syntax/loc stx
       (begin (define (name . args) 
                'this-is-a-stub)
              (provide name)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (rename-out (-#%module-begin #%module-begin)
                     (-#%datum #%datum)
                     (-#%app #%app)
                     (-define define)))
(provide-stub-function printf)
(provide-stub-function *)
