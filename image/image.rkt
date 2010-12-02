#lang s-exp "../lang/js-impl/js-impl.rkt"

(require "../world/kernel.rkt")

(require-js "image.js")

(provide image?
	 image=?
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
	 image-width
	 image-height)
