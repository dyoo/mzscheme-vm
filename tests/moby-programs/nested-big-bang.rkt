#lang s-exp "../../lang/wescheme.rkt"

(define (tock w)
  (printf "tock!\n")
  (js-big-bang w
              (on-tick add1 1)
              (stop-when (lambda (w2) (= w2 (+ w 5))))))


(check-expect (js-big-bang 0 (on-tick tock 5))
	      5)
