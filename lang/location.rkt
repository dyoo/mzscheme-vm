#lang s-exp "kernel.rkt"

;; location structure definition

;; FIXME: replace uses of location with srcloc, so that we match the
;; srcloc structure described in http://docs.racket-lang.org/reference/exns.html?q=srcloc#(def._((lib._racket/private/base..rkt)._srcloc))

(define-struct location (id offset line column span))
(provide (struct-out location))



(define-struct srcloc (source line column position span))
(provide (struct-out srcloc))
