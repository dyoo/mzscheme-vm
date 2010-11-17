#lang s-exp "../js-conditional/js-conditional.rkt"

(declare-conditional-implementation #:racket "racket-display-location.rkt"
                                    #:javascript ("js-display-location.js"))