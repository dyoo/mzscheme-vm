#lang s-exp "../../src/lang/base.ss"

(require "required-4.rkt")

(provide (rename-out (-hypo hypo)) h)

(define (-hypo a b)
  (sqrt (+ (sqr a) (sqr b))))

