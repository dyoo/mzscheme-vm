#lang s-exp "../../lang/base.ss"

(printf "exercise-control.rkt\n")

(check-expect (if true
		  'ok
		  'not-ok)
	      'ok)

(check-expect (if false
		  'not-ok
		  'ok)
	      'ok)

(check-expect (cond [true 'ok]
		    [else 'not-ok])
	      'ok)

(check-expect (cond [false 'not-ok]
		    [else 'ok])
	      'ok)

(check-expect (case 42
		[(1) 'not-ok]
		[(2) 'not-ok]
		[(42) 'ok])
	      'ok)

(check-expect (case 42
		[(1) 'not-ok]
		[(2) 'not-ok]
		[(42) 'ok])
	      'ok)