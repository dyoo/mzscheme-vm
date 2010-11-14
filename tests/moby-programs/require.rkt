#lang s-exp "../../lang/wescheme.rkt"
(require "required.rkt")

(require (prefix-in a-prefix: (only-in "required.rkt" f)))

(require "required-2.rkt")

(printf "require.rkt\n")

(define (blah)
  'blaaargh)

(check-expect (blah) 'blaaargh)

(check-expect (f 42) (* 42 42))

(check-expect (hypo 3 4) 5)

(check-expect (h 16) (expt 16 5))

(check-expect (a-prefix:f 42) (* 42 42))


(check-expect (a-struct-x (make-a-struct 3 4 5)) 3)
(check-expect (a-struct? (make-a-struct 3 4 5)) true)