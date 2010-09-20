#lang s-exp "../../src/lang/moby-lang.rkt"

(require "../../src/jsworld/jsworld.rkt")

(js-big-bang 1 (on-tick add1))