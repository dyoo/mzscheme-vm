#lang s-exp "../../lang/base.rkt"


;; This module requires the majority of tests here.



;; NOTE: js-input.rkt is not being tested here.  Must be handled interactively.
;; NOTE: continuation-prompts.rkt is not being tested here.  Must be handled interactively.
;; NOTE: continuation-prompts-3.rkt is not being tested here.  Must be handled interactively.


(require "with-handlers-1.rkt"
	 "with-handlers-2.rkt"
	 "when-unless.rkt"
	 "setbang.rkt"
	 "require.rkt"
	 "quasiquote.rkt"
	 "permissions.rkt"
	 "local.rkt"
	 "js-big-bang-timer.rkt"
	 "images.rkt"
	 "image-equality.rkt"
	 "falling-ball.rkt"
	 "exercise-control.rkt"
	 "double-client.rkt"
	 "define-struct.rkt"
	 "continuation-prompts-2.rkt"
	 "case-lambda.rkt"
	 "begin.rkt"
	 "and-or.rkt"
	 "42.rkt"	 
	 "cycles.rkt")


(printf "all-tests completed\n")