#lang scheme/base

(define (f x) x)

(if (f #t) 'ok 'not-ok)
