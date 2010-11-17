#lang s-exp "kernel.rkt"

;; location structure definition

(define-struct location (id offset line column span))
(provide (struct-out location))


