#lang s-exp "../../src/lang/moby-lang.ss"

(define (bottles-of-beer)
  (let ([x 100])
    (define (loop)
      (if (< x 1)
	  (void)
	  (begin
	    (set! x (sub1 x))
	    (printf "~a bottles of beer on the wall\n" x)
	    (loop))))
    (loop)))

(bottles-of-beer)