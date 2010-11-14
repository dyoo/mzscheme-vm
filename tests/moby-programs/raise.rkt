#lang s-exp "../../lang/wescheme.rkt"

"raise.rkt"

(check-expect 
 (with-handlers ([string? identity])
   (raise "hello world")
   42)
 "hello world")

"raise.rkt end"