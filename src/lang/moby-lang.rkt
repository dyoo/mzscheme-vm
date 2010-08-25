#lang racket/base
(require (prefix-in math: (only-in racket/math pi))
         (prefix-in math: (only-in mzlib/math e)))

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


;; definitions
(define-syntax (-define stx)
  (syntax-case stx ()
    [(_ x ...)
     (syntax/loc stx
       (define x ...))]))

;; define-struct
(define-syntax (-define-struct stx)
  (syntax-case stx ()
    [(_ x ...)
     (syntax/loc stx
       (define-struct x ...))]))


;; constants
(define true #t)
(define false #f)
(define pi math:pi)
(define e math:e)
(define empty '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive function stubs

;; provide-stub-function
(define-syntax (provide-stub-function stx)
  (syntax-case stx ()
    [(_ name)
     (syntax/loc stx
       (begin (define (name . args) 
                'this-is-a-stub)
              (provide name)))]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
(provide (rename-out (-#%module-begin #%module-begin)
                     (-#%datum #%datum)
                     (-#%app #%app)
                     (-define define)
                     (-define-struct define-struct)
                     (if if)
                     (cond cond)
                     (else else)
                     (case case)
                     (quote quote)
                     (lambda lambda)
                     (let let)
                     (let* let*)
                     (letrec letrec)
                     
                     (true true)
                     (false false)
                     (pi pi)
                     (e e)
                     (empty empty)))

(provide-stub-function printf)
(provide-stub-function format)
(provide-stub-function *)
(provide-stub-function =)
(provide-stub-function sub1)
(provide-stub-function add1)