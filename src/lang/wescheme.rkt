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
                     for-syntax))



;; Pull in the image primitives too.
(require "../image/image.rkt")
(provide (all-from-out "../image/image.rkt"))
;; For compatibility, re-exports image-url as open-image-url.
(define open-image-url image-url)
(provide open-image-url)


;; Pull in the jsworld primitives
(require "../jsworld/jsworld.rkt")
(provide (all-from-out "../jsworld/jsworld.rkt"))