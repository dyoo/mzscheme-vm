#lang s-exp "../../lang/wescheme.rkt"

(printf "images.rkt\n")


(check-expect (image? 'blue) #f)
(check-expect (image? (circle 20 "solid" "green")) #t)

"should be a solid green circle: " (circle 20 "solid" "green")


(check-expect (image=? (circle 50 "solid" "blue")
		       (rectangle 20 30 "outline" "turquoise"))
	      #f)
"should be an outline turquoise rectangle: " (rectangle 20 30 "outline" "turquoise")


;(check-expect (color? (make-color 3 4 5)))

(check-expect (color-red (make-color 3 4 5)) 3)
(check-expect (color-green (make-color 3 4 5)) 4)
(check-expect (color-blue (make-color 3 4 5)) 5)

(check-expect (image? (empty-scene 20 50)) true)
(check-expect (image=? (empty-scene 20 50) (empty-scene 20 50)) true)

(check-expect (image? (place-image (circle 50 'solid 'blue)
				   50
				   50
				   (empty-scene 100 100)))
	      true)

"should be a blue circle in a scene with a border: " (place-image (circle 50 'solid 'blue)
				   50
				   50
				   (empty-scene 100 100))


"should be a text:" (text "hello world" 20 'black)
"should be a text as well:" (text (string-copy "hello world") 20 'black)


"should be a blue ellipse" (ellipse 100 200 "solid" "blue")