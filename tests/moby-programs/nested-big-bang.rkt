#lang s-exp "../../lang/wescheme.rkt"


;; This program does deep big bangs; we expect to see
;; ten levels of tocks.

(define (tock w)
  (printf "tock!\n")
  (js-big-bang (+ w 1)
               (on-tick tock 0.1)
  	       (stop-when (lambda (w2) (>= w2 10)))))


(check-expect (tock 0)
	      10)
