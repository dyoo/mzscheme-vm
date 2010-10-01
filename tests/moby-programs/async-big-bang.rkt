#lang s-exp "../../src/lang/wescheme.rkt"

(define b1 (async-js-big-bang 0 (on-tick add1 1)))
(define b2 (async-js-big-bang 0 (on-tick add1 1/2)))

"Last line"