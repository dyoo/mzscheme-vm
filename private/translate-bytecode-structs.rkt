#lang racket/base

(require "version-case/version-case.rkt"
	 (for-syntax racket/base))

(version-case
 [(and (version<= "5.0.1" (version))
       (version< (version) "5.1"))
  (begin
    (require "translate-bytecode-structs-5.0.1.rkt")
    (provide (all-from-out
	      "translate-bytecode-structs-5.0.1.rkt")))]
 [(version<= "5.1" (version))
  (begin
    (require "translate-bytecode-structs-5.1.rkt")
    (provide (all-from-out
	      "translate-bytecode-structs-5.1.rkt")))]
 #;[(version<= (version) "5.1.1")
  (begin
    (require "translate-bytecode-structs-5.1.1.rkt")
    (provide (all-from-out
	      "translate-bytecode-structs-5.1.1.rkt")))]
 [else
  (error 'js-vm "Currently unsupported version of Racket: ~s" (version))])
