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

"should be an image from a url:" (image-url "http://racket-lang.org/logo.png")
"should be an image from a url:" (open-image-url "http://racket-lang.org/logo.png")



(check-expect (image?
	       (put-pinhole (rectangle 20 20 'solid 'green) 0 0))
	      true)

"should be some overlays"
(overlay 
	 (rectangle 10 20 'solid 'blue)
	 (circle 20 'solid 'green))

"the next two images should be identical"
(overlay (circle 20 "solid" (make-color  50  50 255))
             (square 40 "solid" (make-color 100 100 255)))

(overlay (circle 20 "solid" (make-color  50  50 255))
             (regular-polygon 40 4 "solid" (make-color 100 100 255)))

(overlay (ellipse 10 10 "solid" "red")
             (ellipse 20 20 "solid" "black")
             (ellipse 30 30 "solid" "red")
             (ellipse 40 40 "solid" "black")
             (ellipse 50 50 "solid" "red")
             (ellipse 60 60 "solid" "black"))

"the next two images should be identical"
(overlay (square 20 "solid" (make-color  50  50 255))
             (square 26 "solid" (make-color 100 100 255))
             (square 32 "solid" (make-color 150 150 255))
             (square 38 "solid" (make-color 200 200 255))
             (square 44 "solid" (make-color 250 250 255)))
(overlay (regular-polygon 20 4 "solid" (make-color  50  50 255))
             (regular-polygon 26 4 "solid" (make-color 100 100 255))
             (regular-polygon 32 4 "solid" (make-color 150 150 255))
             (regular-polygon 38 4 "solid" (make-color 200 200 255))
             (regular-polygon 44 4 "solid" (make-color 250 250 255)))
			 
			 
"should be some overlay/xys"
(overlay/xy (rectangle 20 20 "outline" "black")
                20 0
                (rectangle 20 20 "outline" "black"))
(overlay/xy (rectangle 20 20 "solid" "red")
                20 20
                (rectangle 20 20 "solid" "black"))
(overlay/xy (rectangle 20 20 "solid" "red")
                -20 -20
                (rectangle 20 20 "solid" "black"))
(overlay/xy
     (overlay/xy (ellipse 40 40 "outline" "black")
                 10
                 15
                 (ellipse 10 10 "solid" "forestgreen"))
     20
     15
     (ellipse 10 10 "solid" "forestgreen"))
		
"should be two examples of overlay/align"
(overlay/align "middle" "middle"
                   (ellipse 60 30 "solid" "purple")
                   (rectangle 30 60 "solid" "orange"))
(overlay/align "right" "top"
                   (ellipse 60 30 "solid" "purple")
                   (rectangle 30 60 "solid" "orange"))
(overlay/align "left" "bottom"
                   (ellipse 60 30 "solid" "purple")
                   (rectangle 30 60 "solid" "orange"))
				   
(overlay/align "right" "bottom"
                   (rectangle 20 20 "solid" "silver")
                   (rectangle 30 30 "solid" "seagreen")
                   (rectangle 40 40 "solid" "silver")
                   (rectangle 50 50 "solid" "seagreen"))
				   
"should be some underlays"
(underlay (circle 20 'solid 'green)
	  (rectangle 10 20 'solid 'blue))
	  
(underlay (ellipse 10 60 "solid" "red")
              (ellipse 20 50 "solid" "black")
              (ellipse 30 40 "solid" "red")
              (ellipse 40 30 "solid" "black")
              (ellipse 50 20 "solid" "red")
              (ellipse 60 10 "solid" "black"))


"should be an underlay/xy"
(underlay/xy (circle 20 'solid 'green)
	     30 10
	     (rectangle 10 20 'solid 'blue))

"should be some examples of underlay/align"
(underlay/align "middle" "middle"
                   (ellipse 60 30 "solid" "purple")
                   (rectangle 30 60 "solid" "orange"))
(underlay/align "right" "top"
                   (ellipse 60 30 "solid" "purple")
                   (rectangle 30 60 "solid" "orange"))
(underlay/align "left" "bottom"
                   (ellipse 60 30 "solid" "purple")
                   (rectangle 30 60 "solid" "orange"))
				   
(underlay/align "right" "bottom"
                   (rectangle 50 50 "solid" "silver")
                   (rectangle 40 40 "solid" "seagreen")
                   (rectangle 30 30 "solid" "silver")
                   (rectangle 20 20 "solid" "seagreen"))
		 
"should be some examples of beside and beside/align"
(beside (ellipse 20 70 "solid" "gray")
            (ellipse 20 50 "solid" "darkgray")
            (ellipse 20 30 "solid" "dimgray")
            (ellipse 20 10 "solid" "black"))
			
(beside/align "bottom"
                  (ellipse 20 70 "solid" "lightsteelblue")
                  (ellipse 20 50 "solid" "mediumslateblue")
                  (ellipse 20 30 "solid" "slateblue")
                  (ellipse 20 10 "solid" "navy"))
				  
(beside/align "top"
                  (ellipse 20 70 "solid" "mediumorchid")
                  (ellipse 20 50 "solid" "darkorchid")
                  (ellipse 20 30 "solid" "purple")
                  (ellipse 20 10 "solid" "indigo"))
				  		 
"should be some examples of above and above/align"
(above (ellipse 70 20 "solid" "gray")
           (ellipse 50 20 "solid" "darkgray")
           (ellipse 30 20 "solid" "dimgray")
           (ellipse 10 20 "solid" "black"))
										
(above/align "right"
                 (ellipse 70 20 "solid" "gold")
                 (ellipse 50 20 "solid" "goldenrod")
                 (ellipse 30 20 "solid" "darkgoldenrod")
                 (ellipse 10 20 "solid" "sienna"))
(above/align "left"
                 (ellipse 70 20 "solid" "yellowgreen")
                 (ellipse 50 20 "solid" "olivedrab")
                 (ellipse 30 20 "solid" "darkolivegreen")
                 (ellipse 10 20 "solid" "darkgreen"))
				 				 
"Three isosceles triangles of various sizes and fills"

(isosceles-triangle 60 30 "solid" "aquamarine")
(isosceles-triangle 200 170 "outline" "seagreen")
(isosceles-triangle 60 330 "solid" "lightseagreen")


"Three right triangles of various sizes and fills"

(right-triangle 36 48 "solid" "black")
(right-triangle 4 60 "solid" "purple")
(right-triangle 30 40 "outline" "red")

"Three squares of various sizes and fills"

(square 60 "outline" "black")
(square 200 "solid" "seagreen")
(square 100 "outline" "blue")

"Three rhombuses of various sizes and fills"

(rhombus 40 45 "solid" "magenta")
(rhombus 100 200 "solid" "orange")
(rhombus 80 330 "outline" "seagreen")

"Some regular polygons..."
"A triangle of side-length 20: should be 20x17"
(regular-polygon 20 3 "outline" "purple")
"A square of side-length 40: should be 40x40"
(regular-polygon 40 4 "outline" "aquamarine")
"A pentagon of side-length 30: should be 49x46"
(regular-polygon 30 5 "solid" "pink")
"A hexagon of side-length 20: should be 40x35"
(regular-polygon 20 6 "outline" "gold")
"A septagon of side-length 40: should be 90x88"
(regular-polygon 40 7 "outline" "goldenrod")
"An octagon of side-length 30: should be 72x72"
(regular-polygon 30 8 "outline" "darkgoldenrod")
"A nonagon of side-length 20: should be 58x57"
(regular-polygon 20 9 "outline" "sienna")

(printf "Three images at 30, 60, 90 degree rotation:\n")

(rotate 30 (image-url "http://racket-lang.org/logo.png"))
(rotate 60 (image-url "http://racket-lang.org/logo.png"))
(rotate 90 (image-url "http://racket-lang.org/logo.png"))


(printf "scaling small and large")
(scale 1/2 (image-url "http://racket-lang.org/logo.png"))
(scale 2 (image-url "http://racket-lang.org/logo.png"))

(scale/xy 1 2 (image-url "http://racket-lang.org/logo.png"))
(scale/xy 2 1 (image-url "http://racket-lang.org/logo.png"))

"This should be the normal image"
(scale/xy 1 1 (image-url "http://racket-lang.org/logo.png"))


"Rotated, huge image"
(rotate 30 (scale 3 (image-url "http://racket-lang.org/logo.png")))

"From the racket documentation:"
(rotate 45 (ellipse 60 20 "solid" "olivedrab"))
(rotate 5 (rectangle 50 50 "outline" "black"))
(rotate 45
            (beside/align
             "center"
             (rectangle 40 20 "solid" "darkseagreen")
             (rectangle 20 100 "solid" "darkseagreen")))