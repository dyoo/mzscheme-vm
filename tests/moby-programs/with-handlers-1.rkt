#lang s-exp "../../src/lang/moby-lang.rkt"


(with-handlers ([exn:fail? (lambda (exn) 'ok)])
  (/ 1 0))
