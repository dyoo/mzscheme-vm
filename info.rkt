#lang setup/infotab

(define drracket-name "mzscheme-vm")
(define drracket-tools (list (list "src/tool.rkt")))

(define compile-omit-paths '("externals" "support" "lib" "tests" "sandbox"))

(define scribblings '(("doc/jsworld.rkt")
		      ("doc/image.rkt")))
