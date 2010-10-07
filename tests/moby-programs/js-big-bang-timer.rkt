#lang s-exp "../../src/lang/base.rkt"

(require "../../src/jsworld/jsworld.rkt")


(printf "js-big-bang-timer.rkt\n")
(printf "number should be counting up to ten\n")
(check-expect (js-big-bang 1 
			   (on-tick (lambda (w)
				      #;(printf "~s~n" w)
				      (add1 w))
				    1/4)
			   (stop-when (lambda (w) (= w 10))))
	      10)
"All done with js-big-bang-timer!"