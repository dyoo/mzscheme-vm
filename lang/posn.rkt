#lang s-exp "base.rkt"

(define-struct posn (x y) #:mutable)

(provide posn?
	 make-posn
	 posn-x
	 posn-y
	 set-posn-x!
	 set-posn-y!)