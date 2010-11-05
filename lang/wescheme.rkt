#lang s-exp "base.rkt"

;; This is the default language module for WeScheme programs.
;;
;; It implements a "pretty-big"-like language that'll expose everything
;; in one big module, just so users don't have to worry about
;; doing things with require.


(provide (except-out (all-from-out "base.rkt")
                     
                     ;; We provide everything from base except
                     ;; syntax stuff, because it's not quite safe
                     ;; to do arbitrary server-side compilation.
                     define-syntax
                     define-for-syntax
                     begin-for-syntax
                     for-syntax
		     define-struct))

(require (for-syntax racket/base))


(provide (rename-out [-define-struct define-struct]))
(define-syntax (-define-struct stx)
  (syntax-case stx ()
    [(_ struct-id (field-id ...))
     (syntax/loc stx
       (define-struct struct-id (field-id ...) #:mutable))]))




;; Pull in the image primitives too.
(require "../image/image.rkt")
(provide (all-from-out "../image/image.rkt"))

;; For compatibility, re-exports image-url as open-image-url.
(define open-image-url image-url)
(provide open-image-url)

;; Also re-export js-big-bang as big-bang.
(define js-big-bang big-bang)
(provide js-big-bang)

;; re-export jsworld
(require "../jsworld/jsworld.rkt")
(provide (all-from-out "../jsworld/jsworld.rkt"))


;; re-export posn
(require "posn.rkt")
(provide (all-from-out "posn.rkt"))


;; re-export check-expect
(require "check-expect/check-expect.rkt")
(provide (all-from-out "check-expect/check-expect.rkt"))

;; re-export shared
(require "../shared.rkt")
(provide (all-from-out "../shared.rkt"))