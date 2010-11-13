#lang s-exp "../../lang/wescheme.rkt"

"continuation-marks.rkt"

(with-continuation-mark 'x 3
  (check-expect (continuation-mark-set->list 
                   (current-continuation-marks)
                   'x)
		'(3)))

(with-continuation-mark 'x 3
  (with-continuation-mark 'x 4
   (check-expect (continuation-mark-set->list 
                    (current-continuation-marks)
                    'x)
	         '(4))))

"continuation-marks.rkt end"