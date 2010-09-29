#lang racket/base
(require (prefix-in math: (only-in racket/math pi))
         (prefix-in math: (only-in mzlib/math e)))

(require (for-syntax racket/base)
         racket/local)

;; Special forms
(define-syntax (-#%module-begin stx)
  (syntax-case stx ()
    [(_ body ...)
     (syntax/loc stx 
       (#%module-begin body ...))]))

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
  ;; FIXME: restrict define since we don't yet support keywords
  (syntax-case stx ()
    [(_ x ...)
     (syntax/loc stx
       (define x ...))]))

;; define-struct
(define-syntax (-define-struct stx)
  ;; FIXME: restrict define-struct since we don't yet support keywords
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
    [(_ name ...)
     (syntax/loc stx
       (begin (begin (define (name . args) 
                       'this-is-a-stub)
                     (provide name))
              ...))]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provides
(provide (rename-out (-#%module-begin #%module-begin)
                     (-#%datum #%datum)
                     (-#%app #%app)
                     (#%top-interaction #%top-interaction)
                     (#%top #%top)
                     (-define define)
                     (-define-struct define-struct)
                     (if if)
                     (cond cond)
                     (else else)
                     (case case)
                     (quote quote)
                     (unquote unquote)
                     (unquote-splicing unquote-splicing)
                     (lambda lambda)
                     (case-lambda case-lambda)
                     (let let)
                     (let* let*)
                     (letrec letrec)
                     (local local)
                     (quasiquote quasiquote)
                     (begin begin)
                     (begin0 begin0)
                     (set! set!)
                     (and and)
                     (or or)
                     (when when)
                     (unless unless)
                     (require require)
                     (for-syntax for-syntax)
		     (define-for-syntax define-for-syntax)
                     (begin-for-syntax begin-for-syntax)
                     ; (prefix-in prefix-in)
		     ; (only-in only-in)
                     (provide provide)
		     (all-defined-out all-defined-out)
		     (all-from-out all-from-out)
		     (except-out except-out)
		     (rename-out rename-out)    
                     (define-syntax define-syntax)
                     (let/cc let/cc)
		     (with-continuation-mark with-continuation-mark)
                     
                     (true true)
                     (false false)
                     (pi pi)
                     (e e)
                     (empty empty)
                     (eof eof)
                     (null null)))


(provide-stub-function print-values
                       check-expect
                       EXAMPLE
                       check-within
                       write
                       display
                       newline
                       current-print
                       current-continuation-marks
                       continuation-mark-set->list
                       for-each
                       make-thread-cell
                       make-struct-type
                       make-struct-field-accessor
                       make-struct-field-mutator
                       struct-type?
                       struct-constructor-procedure?
                       struct-predicate-procedure?
                       struct-accessor-procedure?
                       struct-mutator-procedure?
                       procedure-arity
		       procedure-arity-includes?
		       make-arity-at-least
		       arity-at-least?
		       arity-at-least-value
                       apply
                       values
                       call-with-values
                       compose
                       current-inexact-milliseconds
                       current-seconds
                       not
                       void
                       random
                       sleep
                       identity
                       raise
                       error

                       make-exn
                       make-exn:fail
                       make-exn:fail:contract
                       make-exn:fail:contract:arity
                       make-exn:fail:contract:variable
                       make-exn:fail:contract:division-by-zero

                       exn-message
                       exn-continuation-marks

		       exn?
		       exn:fail?
		       exn:fail:contract?
		       exn:fail:contract:arity?
		       exn:fail:contract:variable?
		       exn:fail:contract:division-by-zero?


                       *
                       -
                       +
                       =
                       =~
                       /
                       sub1
                       add1
                       <
                       >
                       <=
                       >=
                       abs
                       quotient
                       remainder
                       modulo
                       max
                       min
                       gcd
                       lcm
                       floor
                       ceiling
                       round
                       numerator
                       denominator
                       expt
                       exp
                       log
                       sin
                       cos
                       tan
                       asin
                       acos
                       atan
                       sinh
                       cosh
                       sqr
                       sqrt
                       integer-sqrt
                       make-rectangular
                       make-polar
                       real-part
                       imag-part
                       angle
                       magnitude
                       conjugate
                       sgn
                       inexact->exact
                       exact->inexact
                       number->string
                       string->number
                       xml->s-exp
                       procedure?
                       pair?
                       cons?
                       empty?
                       null?
                       undefined?
		       immutable?
                       void?
                       symbol?
                       string?
                       char?
                       boolean?
                       vector?
                       struct?
                       eof-object?
                       posn?
                       bytes?
                       byte?
                       number?
                       complex?
                       real?
                       rational?
                       integer?
                       exact?
                       inexact?
                       odd?
                       even?
                       zero?
                       positive?
                       negative?
                       box?
                       hash?
                       eq?
                       eqv?
                       equal?
                       equal~?
                       false?
                       boolean=?
                       symbol=?
                       js-object?
                       cons
                       car
                       cdr
                       caar
                       cadr
                       cdar
                       cddr
                       caaar
                       caadr
                       cadar
                       cdaar
                       cdadr
                       cddar
                       caddr
                       cdddr
                       cadddr
                       rest
                       first
                       second
                       third
                       fourth
                       fifth
                       sixth
                       seventh
                       eighth
                       length
                       list?
                       list
                       list*
                       list-ref
                       list-tail
                       append
                       reverse
                       map
                       andmap
                       ormap
                       memq
                       memv
                       member
                       memf
                       assq
                       assv
                       assoc
                       remove
                       filter
                       foldl
                       foldr
                       quicksort
                       sort
                       argmax
                       argmin
                       build-list
                       box
                       box-immutable
                       unbox
                       set-box!
                       make-hash
                       make-hasheq
                       hash-set!
                       hash-ref
                       hash-remove!
                       hash-map
                       hash-for-each
                       make-string
                       replicate
                       string
                       string-length
                       string-ref
                       string=?
                       string-ci=?
                       string<?
                       string>?
                       string<=?
                       string>=?
                       string-ci<?
                       string-ci>?
                       string-ci<=?
                       string-ci>=?
                       substring
                       string-append
                       string->list
                       list->string
                       string-copy
                       string->symbol
                       symbol->string
                       format
                       printf
                       string->int
                       int->string
                       explode
                       implode
                       string-alphabetic?
                       string-ith
                       string-lower-case?
                       string-numeric?
                       string-upper-case?
                       string-whitespace?
                       build-string
                       string->immutable-string
                       string-set!
                       string-fill!
                       make-bytes
                       bytes
                       bytes->immutable-bytes
                       bytes-length
                       bytes-ref
                       bytes-set!
                       subbytes
                       bytes-copy
                       bytes-fill!
                       bytes-append
                       bytes->list
                       list->bytes
                       bytes=?
                       bytes<?
                       bytes>?
                       make-vector
                       vector
                       vector-length
                       vector-ref
                       vector-set!
                       vector->list
                       list->vector
                       build-vector
                       char=?
                       char<?
                       char>?
                       char<=?
                       char>=?
                       char-ci=?
                       char-ci<?
                       char-ci>?
                       char-ci<=?
                       char-ci>=?
                       char-alphabetic?
                       char-numeric?
                       char-whitespace?
                       char-upper-case?
                       char-lower-case?
                       char->integer
                       integer->char
                       char-upcase
                       char-downcase
                       make-posn
                       posn-x
                       posn-y
                       key=?
		       ; on-tick
                       ; on-tick!
                       ; on-key
                       ; on-key!
                       stop-when
                       stop-when!
                       on-redraw
                       on-draw
                       initial-effect
                       js-p
                       js-div
                       js-button
                       js-button!
                       js-input
                       js-img
                       js-text
                       js-select
                       js-big-bang
                       empty-page
                       place-on-page
                       make-world-config
                       make-effect-type
                       effect-type?
                       effect?
                       make-render-effect-type
                       render-effect-type?
                       render-effect?
                       world-with-effects
                       make-render-effect
                       render-effect-dom-node
                       render-effect-effects
                       scheme->prim-js
                       prim-js->scheme
                       procedure->cps-js-fun
                       procedure->void-js-fun
                       js-===
                       js-get-named-object
                       js-get-field
                       js-set-field!
                       js-typeof
                       js-instanceof
                       js-call
                       js-new
                       js-make-hash
                       js-undefined
                       js-null

                       call-with-current-continuation
                       call/cc
		       call-with-continuation-prompt
		       abort-current-continuation
		       default-continuation-prompt-tag
                       make-continuation-prompt-tag)
