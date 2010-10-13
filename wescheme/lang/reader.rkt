#lang s-exp syntax/module-reader

;; http://docs.racket-lang.org/planet/hash-lang-planet.html


#:language (lambda (ip)
	    `(file ,(path->string lang-path)))


(require racket/runtime-path)
(define-runtime-path lang-path "../../lang/wescheme.rkt")