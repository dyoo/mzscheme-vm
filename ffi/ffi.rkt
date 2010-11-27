#lang s-exp "../lang/js-impl/js-impl.rkt"

(require-js "ffi.js")

(provide racket->prim-js
	 prim-js->racket

	 scheme->prim-js  ;; legacy name
	 prim-js->scheme  ;; legacy
	 

	 procedure->cps-js-fun
	 procedure->void-js-fun
	 js-===
	 js-get-global-value
	 js-get-field
	 js-set-field!
	 js-typeof
	 js-instanceof
	 js-call
	 js-new
	 js-make-hash
	 js-undefined
	 js-undefined?
	 js-null
	 js-null?

	 maximum-js-fixnum
	 minimum-js-fixnum



	 js-object?
	 js-value?
	 js-function?
)
