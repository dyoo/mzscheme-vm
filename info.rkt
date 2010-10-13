#lang setup/infotab

(define drracket-name "mzjs")
(define drracket-tools (list (list "private/tool.rkt")))

(define compile-omit-paths '("externals" "support" "lib" "tests" "sandbox"))

(define scribblings '(("doc/jsworld.rkt")
		      ("doc/image.rkt")))
