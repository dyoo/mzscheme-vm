#lang s-exp "../../lang/base.rkt"


(require "../../ffi/ffi.rkt")


(define window
  (js-get-global-value "window"))

(define platform
  (js-get-field window "navigator" "platform"))

(printf "Current browser platform is ~s\n" 
	(prim-js->scheme platform))
