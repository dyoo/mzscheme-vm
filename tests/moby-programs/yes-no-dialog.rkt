#lang s-exp "../../lang/wescheme.rkt"

;; dialog-message: string string string -> boolean
(define (yes-no-dialog msg yes-text no-text)
  ;;  The world is either: "uninitialized", or a boolean.
  (local [(define (draw-page w)
	    (cond
	     [(done? w)
	      (list (js-div))]
	     [else 
	      (list (js-div '(("class" "dialog")))
		    (list (js-text msg))
		    (list (js-button yes '(("class" "button")))
			  (list (js-text yes-text)))
		    (list (js-button no '(("class" "button")))
			  (list (js-text no-text))))]))
          (define (draw-css w)
	    `((".dialog" ("border" "5px"))
	      (".button" ("background-color" "gray"))))
          
          ;; yes: world -> world
          (define (yes w)
            true)
          
          ;; no: world -> world
          (define (no w)
            false)
          
          (define (done? w)
            (boolean? w))]
    (big-bang "uninitialized"
              (to-draw-page draw-page draw-css)
              (stop-when done?))))


(cond [(yes-no-dialog "Sleepy?" 
		      "Yeah, I need a nap!" 
		      "Nope!")
       (cond [(yes-no-dialog "Pillow, or books?" 
			     "Pass the pillow!" 
			     "I want a story!")
	      'pillow]
	     [else
	      'if-you-give-a-mouse-a-cookie])]
      [else
       (cond [(yes-no-dialog "Milk, or cookies?" 
			     "Milk!" 
			     "Cookies!")
	      'cup-of-milk]
	     [else
	      'cookies])])
       
