#lang s-exp "profiled-base.rkt"


(define *HEAT-CUTOFF* 1)

;; find-parameter: number (number -> number) (number -> boolean) -> number
;; Search for a parameter, using the initial guess to start off.
;;
;;
;; Heat should be a function that's nonnegative and 
;; monotonically decreasing that decays eventually to zero.
(define (find-parameter #:initial-guess initial-guess
                        #:heat-function heat
                        #:guess-too-small? too-small?
                        #:max-iterations (max-iterations 100))
  (let loop ([time 0]
             [current-guess initial-guess])
    (let ([h (heat time)])
      (cond
        [(< h *HEAT-CUTOFF*)
         current-guess]
        [(> time max-iterations)
         current-guess]
        [(too-small? current-guess)
         (loop (add1 time)
               (+ current-guess (heat time)))]
        [else
         (loop (add1 time)
               (- current-guess (heat time)))]))))


;; exponential-decay: number #:scale (number 1)
;;                           #:width (number 1) -> number
;; Computes an exponential decay curve; scale and width
;; are meant to stretch out the graph a bit.
(define (exponential-decay #:scale (scale 1) 
                           #:width (width 1)
                           x)
  (* scale (exp (- (/ x width)))))



;; guessing-game: number -> number
;; Drive find-parameter and see if it can figure out n, given 
;; that it knows nothing but whether its guess is too big or small.
(define (guessing-game n)
  (find-parameter #:initial-guess 5000
                  #:heat-function (lambda (x)
                                    (exponential-decay x 
                                                       #:scale 5000
                                                       #:width 20))
                  #:guess-too-small? (lambda (guess)
                                       (< guess n))))
