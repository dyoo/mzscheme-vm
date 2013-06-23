#lang s-exp "../../lang/wescheme.rkt"

(define video (video-url "http://www.youtube.com/demo/google_main.mp4"))
(define image (image-url "http://racket-lang.org/logo.png"))

(define (draw-world w)
	(rotate w	(overlay (rotate (modulo (* w 2) 360) image) 
				video)))
		
(define (update-world w)
	(modulo (+ w 1) 360))
	
(js-big-bang 0
	(on-tick update-world .05)
	(to-draw draw-world))