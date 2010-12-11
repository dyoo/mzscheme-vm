#lang s-exp "../lang/js-impl/js-impl.rkt"

(require "../world/kernel.rkt")
(require-js "image.js")

(provide 
	 make-color
	 color-red
	 color-green
	 color-blue
	 empty-scene
	 scene+line
	 place-image
	 place-image/align
	 put-pinhole
	 circle
	 star
	 radial-star
	 star-polygon
	 nw:rectangle
	 rectangle
	 regular-polygon
	 rhombus
	 square
	 triangle
	 right-triangle
	 isosceles-triangle
	 ellipse
	 line
	 add-line
	 overlay
	 overlay/xy
	 overlay/align
	 underlay
	 underlay/xy
	 underlay/align
	 beside
	 beside/align
	 above
	 above/align
	 rotate
	 scale
	 scale/xy
	 crop
	 frame
	 flip-horizontal
	 flip-vertical
	 text
	 text/font
	 video-url
	 image-url
	 image?
	 image=?
	 image-width
	 image-height

	 image->color-list
	 color-list->image

	 image-baseline
	 mode?
	 image-color?
	 x-place?
	 y-place?
	 angle?
	 side-count?
	 step-count?)
