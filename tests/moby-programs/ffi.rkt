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
(check-expect (prim-js->scheme (scheme->prim-js #t))
	      #t)
(check-expect (prim-js->scheme (scheme->prim-js #f))
	      #f)


;; (prim-js->scheme (scheme->prim-js ...)) is not the identity, unfortunately.
;; Here are tests that show that we need to do something:

;; Numbers that come back to us are inexact.
(check-expect (inexact->exact (prim-js->scheme (scheme->prim-js 42)))
	      42)

;; Characters are mapped to one-character strings.
(check-expect (prim-js->scheme (scheme->prim-js #\h))
	      "h")

;; Symbols are mapped to strings.
(check-expect (prim-js->scheme (scheme->prim-js 'hello))
	      "hello")

;; Note that when converting vectors, the resulting inner values are not
;; automatically transformed back.  So the prim-js->scheme transformation 
;; is shallow.
(check-expect (map prim-js->scheme 
		   (vector->list (prim-js->scheme (scheme->prim-js #(1 2 3)))))
	      '(1.0 2.0 3.0))

(check-expect (map prim-js->scheme 
		   (vector->list (prim-js->scheme 
				  (scheme->prim-js #(1 "testing" 3)))))
	      '(1.0 "testing" 3.0))



(check-expect (js-=== js-undefined js-undefined) true)
(check-expect (js-=== js-null js-null) true)
(check-expect (js-=== js-undefined js-null) false)
(check-expect (js-=== js-null js-false) false)