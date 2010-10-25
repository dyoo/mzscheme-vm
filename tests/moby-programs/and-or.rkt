#lang s-exp "../../lang/wescheme.ss"

(printf "and-or.rkt\n")

(check-expect (and true "hello") "hello")
(check-expect (or #f #f "world" 'dontcomehere)
	      "world")
