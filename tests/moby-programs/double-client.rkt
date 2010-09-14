#lang s-exp "../../src/lang/moby-lang.rkt"

(require "double.rkt")

(double 3)

(double (double (double 2)))

