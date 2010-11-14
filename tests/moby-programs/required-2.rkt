#lang s-exp "../../lang/base.ss"

(require "required-3.rkt")

(provide hypo
	 h)


(define-struct a-struct (x y z))
(provide (struct-out a-struct))