#lang s-exp "../../lang/wescheme.rkt"

(define mylst (cons 1 (cons (box #f) empty)))
(set-box! (cadr mylst) mylst)

(check-expect (format "~s\n" mylst)
	      "(1 #&...)\n")


(shared ([a (cons 1 a)])
  (check-expect (format "~s\n"a)
	        "(1 ...)\n"))
