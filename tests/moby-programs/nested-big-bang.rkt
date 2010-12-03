#lang s-exp "../../lang/wescheme.rkt"


"nested-big-bang.rkt"

;; This program does deep big bangs; we expect to see
;; ten levels of tocks.

(define MAX-LEVEL 100  
	          #;1000    ;; stress test the nesting with this
)

(define (tock w)
  (js-big-bang (+ w 1)
               (on-tick tock 0.00001)
  	       (stop-when (lambda (w2) (>= w2 MAX-LEVEL)))))


(check-expect (tock 0)
	      MAX-LEVEL)
