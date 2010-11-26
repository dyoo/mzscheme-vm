#lang s-exp "../../lang/base.rkt"
(require "../../ffi/ffi.rkt"
	 "../../jsworld/jsworld.rkt"
	 "../../lang/check-expect/check-expect.rkt")

"my-ffi-2.rkt"

;; Check to see that we can expression on-tick with make-world-config.

(define setInterval (js-get-global-value "setInterval"))
(define clearInterval (js-get-global-value "clearInterval"))


(define (my-on-tick world-updater)
  (make-world-config
   (lambda (tick)
     (js-call setInterval #f
              (procedure->void-js-fun 
               (lambda args 
                 (js-call tick #f)))
              0))

   (lambda (id)
     (printf "shutdown with clearInterval id=~s\n" id)
     (js-call clearInterval #f id))

   (lambda (w)
     (world-updater w))))




(check-expect (big-bang 1
			(my-on-tick 
			 (lambda (w)
			   ;(printf "tick!\n")
			   (add1 w)))

			(stop-when 
			 (lambda (n) (= n 2))))
	      2)


(run-tests)
"end my-ffi-2.rkt"