#lang s-exp "../../src/lang/base.rkt"


(define (f x)
  (* x x))

(printf "~s ~s ~s\n" 
        (f 16) 
        (f -5)
        (f 42))
