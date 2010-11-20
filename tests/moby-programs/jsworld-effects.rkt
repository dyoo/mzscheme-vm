#lang s-exp "../../lang/wescheme.rkt"
(require "../../jsworld/define-effect.rkt")

(define-effect effect:beep ()
  #:impl (lambda (w)
           (printf "Beep!")))