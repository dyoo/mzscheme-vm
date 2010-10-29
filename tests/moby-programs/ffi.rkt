#lang s-exp "../../lang/base.rkt"


(require "../../ffi/ffi.rkt"
	 "../../lang/check-expect/check-expect.rkt")


(define window
  (js-get-global-value "window"))

(define platform
  (js-get-field window "navigator" "platform"))

(printf "Current browser platform is ~s\n" 
	(prim-js->scheme platform))


(check-expect (prim-js->scheme (scheme->prim-js "hello world"))
	      "hello world")
(check-expect (prim-js->scheme (scheme->prim-js 42))
	      42)
(check-expect (prim-js->scheme (scheme->prim-js #t))
	      #t)
(check-expect (prim-js->scheme (scheme->prim-js #f))
	      #f)