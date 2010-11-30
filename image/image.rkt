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
	 place-image
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
	 flip-horizontal
	 flip-vertical
	 text
	 video-url
	 image-url
	 image-width
	 image-height)
