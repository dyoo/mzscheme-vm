#lang s-exp "../../src/lang/base.rkt"


(with-handlers ([(lambda (exn)
		   (printf "Is the exception a failure? ~s~n" (exn:fail? exn))
		   (exn:fail? exn))
 	         (lambda (exn)
		   (printf "I'm in the handler and saying ok\n")
		   'ok)])
  (/ 1 0))				   



(with-handlers ([(lambda (exn)
		   false)
 	         (lambda (exn)
		   (printf "I'm in the handler and saying ok\n")
		   'not-ok)]
		[(lambda (exn)
		   (printf "second test\n")
		   true)
		 (lambda (exn)
		   'ok)])
  (/ 1 0))				   



(with-handlers ([void (lambda (exn) 'not-ok)])
  'ok)				   


(/ 1 0)