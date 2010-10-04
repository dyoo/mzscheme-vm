#lang s-exp "../../src/lang/base.rkt"

(require "../../src/jsworld/jsworld.rkt")


(js-big-bang 1 
	     (on-tick (lambda (w)
	                (printf "~s~n" w)
	                (add1 w))
                      1)
             (stop-when (lambda (w) (= w 10))))


"All done!"