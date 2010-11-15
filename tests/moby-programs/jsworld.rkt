#lang s-exp "../../lang/wescheme.rkt"

(require "../../jsworld/jsworld.rkt")
;; more jsworld tests

(printf "jsworld.rkt\n")

(check-expect (js-big-bang 1 (stop-when (lambda (x) true)))
	      1)


"should be an empty page"
(void (big-bang 1
		(to-draw-page (lambda (x) 
				(empty-page))
			      (lambda (x) '()))
		(stop-when (lambda (x) true))))
      

"should be another empty page"
(void (big-bang 1
		;; check single-arity to-draw-page
		(to-draw-page (lambda (x) 
				(empty-page)))
		(stop-when (lambda (x) true))))


"at this point, something should be moving on the screen"
(void (big-bang 1
		(on-tick (lambda (x) (+ x 5)))
		;; check single-arity to-draw-page
		(to-draw-page (lambda (x) 
				(place-on-page
				 (if (>= x 500)
				     "I'm done"
				     "I'm moving!")
					;(circle 100 'solid 'blue)
				 (modulo x 300)
				 (modulo x 300)
				 (empty-page))))
		(stop-when (lambda (x) (>= x 500)))))

      

(check-expect (image-height (circle 20 'solid 'green))
	      40)


(check-expect (image-width (circle 30 'solid 'green))
	      60)


(check-expect (effect-type? 42) false)
(check-expect (effect? 42) false)
#;(check-expect (effect? '()) true)


"a button"
(void (big-bang 1
		(to-draw-page (lambda (x) 
				(list
				 (js-button (lambda (x) x))
				 (list (js-text "a button")))))
		(stop-when (lambda (x) true))))
"another button"
(void (big-bang 1
		(to-draw-page (lambda (x) 
				(list
				 (js-button! (lambda (x) x)
					     (lambda (x) '()))
				 (list (js-text "another button")))))
		(stop-when (lambda (x) true))))





"jsworld.rkt end"