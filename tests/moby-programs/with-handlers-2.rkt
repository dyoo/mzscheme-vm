#lang s-exp "../../src/lang/base.rkt"


(with-handlers ([void (lambda (exn) 'ok)])
  (with-handlers ([1 2])
    (/ 1 0)))


(with-handlers ([void (lambda (exn) 'ok)])
  (with-handlers ([void 2])
    (/ 1 0)))

(with-handlers ([void (lambda (exn)
	                (printf "outer\n")
			 'not-ok)])
  (with-handlers ([void (lambda (exn)
                           (printf "inner\n")
			    'ok)])
    (/ 1 0)))