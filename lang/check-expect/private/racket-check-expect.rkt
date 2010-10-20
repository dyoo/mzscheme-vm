#lang racket/base

(define (check-expect test expected)
  (cond [(equal? test expected)
         (void)]
        [else 
         (error 'check-expect 
                "Expected ~s, but got ~s" expected test)]))


(define (check-within test expected delta)
  (cond [(<= (- expected delta) test (+ expected delta))
         (void)]
        [else
         (error 'check-within 
                "Expected ~s (+- ~s), but got ~s" expected delta test)]))


;;(define (check-error* error-thunk expected-message)
;;  'fill-me-in)


(provide check-expect check-within #;check-error*)