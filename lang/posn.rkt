#lang s-exp "base.rkt"

(define-struct posn (x y))

(provide posn?
	 make-posn
	 posn-x
	 posn-y)