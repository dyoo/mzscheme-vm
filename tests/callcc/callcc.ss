#lang scheme/base
(call/cc (lambda (k)
           (+ 1 2 (k 17))))