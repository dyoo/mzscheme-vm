#lang setup/infotab

(define drracket-name "mzjs")
(define drracket-tools (list (list "private/tool.rkt")))

(define compile-omit-paths '("externals" "support" "lib" "tests" "sandbox"))

(define scribblings '(("doc/jsworld.scrbl")
		      ("doc/image.scrbl")))


(define categories '(devtools))
(define can-be-loaded-with 'all)
(define required-core-version "5.0.1")
(define version "1.0alpha")
(define repositories '("4.x"))
(define blurb 
  '("Provides support for compiling Racket programs to run as Javascript on the browser."))