#lang s-exp "../../lang/wescheme.rkt"

(check-expect (identity 42) 42)
(check-expect (identity +) +)