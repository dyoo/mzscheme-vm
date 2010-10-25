#lang s-exp "../../js-conditional/js-conditional.rkt"

(declare-conditional-implementation 
 #:racket "racket-check-expect.rkt"
 #:javascript ("js-check-expect.js"))