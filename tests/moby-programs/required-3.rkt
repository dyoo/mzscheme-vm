#lang s-exp "../../src/lang/moby-lang.ss"

(require "required-4.rkt")

(provide (rename-out (-hypo hypo)) h)

(define (-hypo a b)
  (sqrt (+ (sqr a) (sqr b))))

