#lang s-exp "../../src/lang/wescheme.rkt"



(image? 'blue)
(image? (circle 20 "solid" "green"))


(image=? (circle 50 "solid" "blue")
	 (rectangle 20 30 "outline" "turquoise"))

(make-color 3 4 5)

(check-expect (color-red (make-color 3 4 5)) 3)
(check-expect (color-green (make-color 3 4 5)) 4)
(check-expect (color-blue (make-color 3 4 5)) 5)

(empty-scene 20 50)
(image? (empty-scene 20 50))
(check-expect (image=? (empty-scene 20 50) (empty-scene 20 50)) true)

(place-image (circle 50 'solid 'blue)
	     50
	     50
	     (empty-scene 100 100))