#lang s-exp "../../src/lang/moby-lang.ss"
(local [(define (f x)
	  (* x x))
	(define (g x)
	  (* x x x))]
  (f (g (g (f 3)))))
      