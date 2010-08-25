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

;; branches
(define-syntax (-if stx)
  (syntax-case stx ()
    [(_ t b1 b2)
     (syntax/loc stx
       (if t b1 b2))]))

;; conds
(define-syntax (-cond stx)
  (syntax-case stx ()
    [(_ clauses ...)
     (syntax/loc stx
       (cond clauses ...))]))

;; case
(define-syntax (-case stx)
  (syntax-case stx ()
    [(_ clauses ...)
     (syntax/loc stx
       (case clauses ...))]))


;; definitions
(define-syntax (-define stx)
  (syntax-case stx ()
    [(_ x ...)
     (syntax/loc stx
       (define x ...))]))

;; quotations
(define-syntax (-quote stx)
  (syntax-case stx ()
    [(_ x ...)
     (syntax/loc stx
       (quote x ...))]))

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
                     (-if if)
                     (-cond cond)
                     (else else)
                     (-case case)
                     (-quote quote)
                     
                     (true true)
                     (false false)
                     (pi pi)
                     (e e)
                     (empty empty)))

(provide-stub-function printf)
(provide-stub-function *)
