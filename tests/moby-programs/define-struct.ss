#lang s-exp "../../src/lang/moby-lang.ss"


(define-struct p (x y))
(printf "~s~n" (make-p 3 4))
(printf "~s~n" (p? (make-p 3 4)))
(printf "~s~n" (p-x (make-p 3 4)))
(printf "~s~n" (p-y (make-p 3 4)))
