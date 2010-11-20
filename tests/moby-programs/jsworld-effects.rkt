#lang s-exp "../../lang/wescheme.rkt"
(require "../../jsworld/define-effect.rkt")

(define-effect effect:beep ()
  #:impl (lambda (w)
           (printf "Beep!")))

"This is an effect: " (make-effect:beep)

(check-expect (effect? (make-effect:beep)) #t)
(check-expect (effect-type? (make-effect:beep)) #f)
(check-expect (effect-type? struct:effect:beep) #t)
(check-expect (effect-type? (make-effect:beep)) #f)



(big-bang 0
          (on-tick (lambda (w)
                     (add1 w))
                   1)
          (initial-effect (make-effect:beep))
          (stop-when (lambda (w) (= w 5))))