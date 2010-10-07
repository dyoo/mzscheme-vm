#lang s-exp "../../src/lang/base.rkt"

(require "double.rkt")

(check-expect (double 3) 6)

(check-expect (double (double (double 2))) 16)

