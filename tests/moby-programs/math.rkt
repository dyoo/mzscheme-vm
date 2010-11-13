#lang s-exp "../../lang/wescheme.rkt"

"math.rkt"

(check-expect (number? pi) true)
(check-expect (number? e) true)

(check-within pi 22/7 0.1)
(check-within e 2.718 0.1)


"math.rkt end"