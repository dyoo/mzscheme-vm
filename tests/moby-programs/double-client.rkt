#lang s-exp "../../src/lang/base.rkt"

(require "double.rkt")

(double 3)

(double (double (double 2)))

