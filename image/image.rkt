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
	 rhombus
	 square
	 triangle
	 right-triangle
	 isosceles-triangle
	 ellipse
	 line
	 overlay
	 overlay/xy
	 underlay
	 underlay/xy
	 rotate
	 scale
	 scale/xy
	 text
	 image-url
	 image-width
	 image-height)
