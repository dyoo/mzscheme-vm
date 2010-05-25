#lang scheme/base

(define-struct posn (x y) #:transparent)
(posn-x (make-posn 3 4))
(posn-y (make-posn 3 4))

(posn? 3)
(posn? (make-posn 7 8))