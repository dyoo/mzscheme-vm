#lang s-exp "../../lang/wescheme.rkt"
(require "required.rkt")
(require "required-2.rkt")

(printf "require.rkt\n")

(define (blah)
  'blaaargh)

(check-expect (blah) 'blaaargh)

(check-expect (f 42) (* 42 42))

(check-expect (hypo 3 4) 5)

(check-expect (h 16) (expt 16 5))