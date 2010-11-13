#lang s-exp "../../lang/wescheme.rkt"

"values.rkt"

(call-with-values (lambda () (values 3 4 5))
                             (lambda (x y z)
			       (check-expect x 3)
			       (check-expect y 4)
			       (check-expect z 5)))

(call-with-values (lambda () (values 3 4 5))
                             (lambda args
			       (check-expect args '(3 4 5))))

(call-with-values (lambda () (values))
                             (lambda ()
			       (void)))



"values.rkt end"