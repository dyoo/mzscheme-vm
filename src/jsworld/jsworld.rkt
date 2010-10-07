#lang s-exp "../lang/js-impl/js-impl.rkt"

;; Loaded so we have access to image library stuff, as well as the world kernel
(require "../world/kernel.rkt"
	 "../image/image.rkt")


(require-js "private/jsworld/jsworld.js"
	    "private/jsworld.js"
	    "world-config.js"
	    "world-stimuli.js"
	    "jsworld.js")


(provide key=?
	 on-tick on-tick!
	 on-key on-key!
	 stop-when stop-when!
	 on-redraw on-draw
	 initial-effect

	 js-p
	 js-div
	 js-button
	 js-button!
	 js-input
	 js-img
	 js-text
	 js-select
	 js-big-bang

	 empty-page
	 place-on-page

	 make-world-config
	 make-effect-type
	 effect-type?
	 effect?

	 make-render-effect-type
	 render-effect-type?

	 world-with-effects

	 make-render-effect
	 render-effect?
	 render-effect-dom-node
	 render-effect-effects)
