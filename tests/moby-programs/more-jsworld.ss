#lang s-exp "../../lang/wescheme.rkt"

;; Fill me in with automated tests for jsworld...
;; This file is intentionally with an '.ss' suffix
;; to see if we've also fixed an issue with module names.


;; Thanks to William Zimrin and Tiberiu-Lucian Florea
;; for this test case.
(big-bang empty
          (to-draw-page 
	   (lambda (x)
	     (list (js-div '(("id" "div")))
		   (list (js-text "hello"))))
	   (lambda (x)
	     `((,(string-append "div") ("border" "3px black solid")))))
	  (stop-when (lambda (x) true)))