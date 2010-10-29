#lang setup/infotab

(define drracket-name "mzjs")
#;(define drracket-tools (list (list "private/tool.rkt")))

(define compile-omit-paths '("externals" "support" "lib" "tests" "sandbox"))

(define scribblings '(("js-vm.scrbl")))

(define primary-file "main.rkt")
(define categories '(devtools))
(define can-be-loaded-with 'all)
(define required-core-version "5.0.1")
(define version "1.3")
(define repositories '("4.x"))
(define blurb 
  '("Provides support for compiling Racket programs to run as Javascript on the browser."))
(define release-notes
  '("Experimental release; no documentation yet."))