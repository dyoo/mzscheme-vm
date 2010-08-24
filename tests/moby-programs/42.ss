#lang s-exp "../../src/lang/moby-lang.rkt"


(define (f x)
  (* x x))

(printf "~s ~s\n" 
        (f 16) 
        (f -5)
        (f 42))
