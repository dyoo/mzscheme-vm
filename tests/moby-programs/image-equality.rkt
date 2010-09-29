#lang s-exp "../../src/lang/moby-lang.rkt"
(require "../../src/image/image.rkt")

;; Tests on images.
;;
;; An image can be a circle, star, ns:rectangle, rectangle, triangle, ellipse, line, text, place-image, overlay, underlay

(define a-circle (circle 20 'solid 'green))
(define a-star (star 5 20 30 'solid 'blue))
(define a-nw-rect (nw:rectangle 20 30 'solid 'turquoise))
(define a-rect (rectangle 50 60 'outline 'black))
(define a-triangle (triangle 50 'solid 'magenta))
(define a-line (line 30 40 'red))
(define a-text (text "hello world" 20 "black"))


;; Let's show these at the toplevel to make sure the drawing is working
;; ok
a-circle
a-star
a-nw-rect
a-rect
a-triangle
a-line
a-text


;; check-fail-contract: (-> void) -> void
;; Make sure we fail with a contract error.
(define (check-fail-contract thunk)
  (with-handlers ([exn:fail:contract? void])
    (thunk)))


;; Now do image comparisons

;; circles
(check-expect (equal? (circle 20 'solid 'green)
                      (circle 20 'solid 'green))
              true)
(check-expect (equal? (circle 20 'solid 'green)
                      (circle 21 'solid 'green))
              false)
(check-expect (equal? (circle 20 'solid 'green)
                      (circle 20 'solid 'blue))
              false)
(check-fail-contract (lambda () (circle 20 "foobar" "green")))
(check-fail-contract (lambda () (circle 20 'green 'outline)))



;; star
(check-expect (equal? (star 20 10 60 'solid 'purple)
                      (star 20 10 60 'solid 'purple))
              true)
(check-expect (equal? (star 20 9 60 'solid 'purple)
                      (star 20 10 60 'solid 'purple))
              false)
(check-expect (equal? (star 20 10 60 'solid 'purple)
                      (star 20 10 49 'solid 'purple))
              false)
(check-expect (equal? (star 20 10 60 'solid 'purple)
                      (star 20 10 60 'outline 'purple))
              false)
(check-expect (equal? (star 20 10 60 'solid 'purple)
                      (star 20 10 60 'solid 'magenta))
              false)
(check-fail-contract (lambda () (star 20 10 60 "foobar" 'green)))
(check-fail-contract (lambda () (star 20 10 60 "green" 'outline)))
 

;; nw:rect
(check-expect (equal? (nw:rectangle 10 20 'solid 'black)
                      (nw:rectangle 10 20 'solid 'black))
              true)
(check-expect (equal? (nw:rectangle 20 10 'solid 'black)
                      (nw:rectangle 10 20 'solid 'black))
              false)
(check-expect (equal? (nw:rectangle 10 10 'solid 'black)
                      (nw:rectangle 10 20 'solid 'black))
              false)
(check-expect (equal? (nw:rectangle 10 20 'solid 'black)
                      (nw:rectangle 10 20 'outline 'black))
              false)
(check-expect (equal? (nw:rectangle 10 20 'solid 'black)
                      (nw:rectangle 10 20 'outline 'white))
              false)
(check-fail-contract (lambda () (nw:rectangle 10 20 "foobar" 'green)))
(check-fail-contract (lambda () (nw:rectangle 10 20 'green 'outline)))


;; rect
(check-expect (equal? (rectangle 10 20 'solid 'black)
                      (rectangle 10 20 'solid 'black))
              true)
(check-expect (equal? (rectangle 20 10 'solid 'black)
                      (rectangle 10 20 'solid 'black))
              false)
(check-expect (equal? (rectangle 10 10 'solid 'black)
                      (rectangle 10 20 'solid 'black))
              false)
(check-expect (equal? (rectangle 10 20 'solid 'black)
                      (rectangle 10 20 'outline 'black))
              false)
(check-expect (equal? (rectangle 10 20 'solid 'black)
                      (rectangle 10 20 'outline 'white))
              false)
(check-fail-contract (lambda () (rectangle 10 20 "foobar" 'green)))
(check-fail-contract (lambda () (rectangle 10 20 'green 'outline)))


;; triangle

;; line

;; text