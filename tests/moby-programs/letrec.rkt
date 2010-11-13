#lang s-exp "../../lang/wescheme.rkt"

"letrec"

(letrec ([even? (lambda (x)
		  (if (= x 0)
		      true
		      (odd? (sub1 x))))]
	 [odd? (lambda (x)
		 (if (= x 0)
		     false
		     (even? (sub1 x))))])
  (check-expect (even? 1024) true)
  (check-expect (even? 1023) false)
  (check-expect (even? 2172) true)
  (check-expect (even? 2171) false))




(letrec-values ([(even? odd?)
		 (values
		  (lambda (x)
		    (if (= x 0)
			true
			(odd? (sub1 x))))
		  (lambda (x)
			 (if (= x 0)
			     false
			     (even? (sub1 x)))))])
  (check-expect (even? 1024) true)
  (check-expect (even? 1023) false)
  (check-expect (even? 2172) true)
  (check-expect (even? 2171) false))





(letrec ([fact (lambda (x)
		 (if (= x 0)
		     1
		     (* x (fact (sub1 x)))))])
  (check-expect (fact 3) 6)
  (check-expect (fact 4) 24)
  (check-expect (fact 5) 120))


"letrec.rkt end"
