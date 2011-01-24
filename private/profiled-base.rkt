#lang racket/base

;; This module provides a variation of racket/base that
;; records allocations for certain constructors.

(require (for-syntax racket/base))

(provide (except-out (all-from-out racket/base)
                     cons
                     list
                     make-hash
                     string-append
                     open-output-string
                     format
                     fprintf))


;; I want to count allocations to see what things are expensive.


(define (record name . args)
  (printf "~s\n" (cons name args)))


(define-syntax (define/provide-wrapped-constructor stx)
  (syntax-case stx ()
    [(_ internal-name)
     (with-syntax ([(hidden-name)
                    (generate-temporaries #'(internal-name))])
       (syntax/loc stx
         (begin
           (provide (rename-out [hidden-name internal-name]))
           (define-syntax (hidden-name stx2)
             (syntax-case stx2 ()
               [(_ args (... ...))
                (syntax/loc stx2
                  (begin
                    #;(record 'internal-name)
                    (internal-name args (... ...))))])))))]))


(define/provide-wrapped-constructor cons)
(define/provide-wrapped-constructor list)
(define/provide-wrapped-constructor make-hash)
(define/provide-wrapped-constructor string-append)
(define/provide-wrapped-constructor open-output-string)
(define/provide-wrapped-constructor format)
(define/provide-wrapped-constructor fprintf)


#;(define-syntax (my-define-struct stx)
  (syntax-case stx ()
    [(_ args ...)
     (syntax/loc stx
       (begin
         (define-struct args ...)))]))
