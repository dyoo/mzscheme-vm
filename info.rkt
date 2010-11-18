#lang setup/infotab

#;(define drracket-name "js-vm")
#;(define drracket-tools (list (list "private/tool.rkt")))

(define compile-omit-paths '("externals" "support" "lib" "tests" "sandbox" "tmp"))

(define scribblings '(("js-vm.scrbl" (multi-page))))
(define name "js-vm")
(define primary-file "main.rkt")
(define categories '(devtools))
(define can-be-loaded-with 'all)
(define required-core-version "5.0.1")
(define version "1.10")
(define repositories '("4.x"))
(define blurb 
  '("Provides support for compiling Racket programs to run as Javascript on the browser."))

(define release-notes
  '(
    (p "Fixes:"
       (ul

	(li "check-expect: check-expect now works by
    applying tests after all of the definitions and other expressions
    have evaluated.")
	
	(li "jsworld reentrancy: there was an issue where jsworld behaved badly
    (reverting to old worlds) if events came in at a rate faster than
    it could handle them.")
	
	(li "member: the definition of member can been corrected to return a
    boolean in the WeScheme (Moby) languages.")
	
	(li "miscellaneous fixes: corrected errors involving
    struct-mutator-procedure, exn structures constructors, and some
    arithmetic edge cases.")
	(li "exposed the struct-out form for provides.")))))
