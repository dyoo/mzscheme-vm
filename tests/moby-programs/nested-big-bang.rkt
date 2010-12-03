#lang s-exp "../../lang/wescheme.rkt"


;; This program does deep big bangs; we expect to see
;; ten levels of tocks.

(define MAX-LEVEL 10)

(define (tock w)
  (js-big-bang (+ w 1)
               (on-tick tock 0.1)
  	       (stop-when (lambda (w2) (>= w2 MAX-LEVEL)))))


(check-expect (tock 0)
	      MAX-LEVEL)
