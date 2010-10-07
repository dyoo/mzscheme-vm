#lang s-exp "../../src/lang/moby-lang.ss"


(define name "danny")
(define roommates (list "guillaume" "isis" "andy"))

`(my name is ,name and I lived with ,@roommates)