#lang racket/base

(define (check-expect test expected)
  (when (procedure? expected)
    (error 'check-expect "cannot compare functions"))
  (cond [(equal? test expected)
         (void)]
        [else 
         (error 'check-expect 
                "Expected ~s, but got ~s" expected test)]))


(define (check-within test expected delta)
  (when (not (real? delta))
    (error 'check-within 
	   "requires an inexact number for the range.  ~s is not inexact."
	   delta))
  (cond [(and (real? test)
	      (real? expected)
	      (<= (- expected delta) test (+ expected delta)))
         (void)]
        [else
         (error 'check-within 
                "Expected ~s (+- ~s), but got ~s" expected delta test)]))



(provide check-expect check-within)