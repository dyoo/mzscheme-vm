#lang s-exp "../../lang/wescheme.rkt"

"arity.rkt"
(check-expect (procedure-arity (lambda () (void))) 0)
(check-expect (procedure-arity (lambda (x) (void))) 1)
(check-expect (procedure-arity (lambda (x y . z) (void))) 
              (make-arity-at-least 2))

(check-expect (arity-at-least-value
               (make-arity-at-least 7))
              7)

"arity.rkt end"