#lang scribble/manual

@(require (for-label "../src/jsworld/jsworld.rkt"))


@title{jsworld}

@defmodule[jsworld]

jsworld provides a world programming library that allows simple animation and games, as well
as reactive HTML graphical user interfaces.

@section{Examples}


@section{Functions}


@defproc[(big-bang (a-world world) (handlers handler) ...) void]{
Starts a reactive computation with @scheme[big-bang].
The rest of the arguments hook into the reactive computation.

By default, the page that's displayed contains a rendering of the
world value.  In the presence of an @scheme[on-draw] or
@scheme[to-draw] handler, @scheme[big-bang] will show a
customized view.

The majority of the handlers register different stimuli that can
trigger changes to the world.  One instance is @scheme[on-tick], which
registers a function to update the world on a clock tick.  }



@defproc[(to-draw-page [to-dom (world -> (DOM-sexp))]
		       [to-css (world -> (CSS-sexp))]) handler]{

One of the main handlers to @scheme[big-bang] is @scheme[to-draw-page],
which controls how the world is rendered on screen.  The first
argument computes a rendering of the world as a DOM tree, and the
second argument computes that tree's styling.  }


                                                                                                                 
@defproc[(to-draw [hook (world -> scene)]) handler]{
For simple applications, @scheme[to-draw] is sufficient to draw a scene onto the display.
The following program shows a ball falling down a scene.

@(schemeblock
(define WIDTH 320)
(define HEIGHT 480)
(define RADIUS 15)

(define INITIAL-WORLD 0)

(define (tick w)
  (+ w 5))

(define (hits-floor? w)
  (>= w HEIGHT))

(check-expect (hits-floor? 0) false)
(check-expect (hits-floor? HEIGHT) true)

(define (render w)
  (place-image (circle RADIUS "solid" "red") (/ WIDTH 2) w
               (empty-scene WIDTH HEIGHT)))

(big-bang INITIAL-WORLD
             (on-tick tick 1/15)
             (to-draw render)
             (stop-when hits-floor?)))
}


@defproc[(stop-when [stop? (world -> boolean)]) handler?]{
When the world should be stopped --- when @scheme[stop?] applied to the world
produces @scheme[true] --- then the @scheme[big-bang] terminates.

The program:
@(schemeblock
(define (at-ten x)
  (>= x 10))

(big-bang 0
             (on-tick add1 1)
             (stop-when at-ten))
)
counts up to ten and then stops.
}

























API

big-bang

key=?
Same as on-key:
http://docs.racket-lang.org/teachpack/2htdpuniverse.html?q=on-tick#(def._((lib._2htdp/universe..rkt)._key~3d~3f))


on-tick

Same as on-tick: http://docs.racket-lang.org/teachpack/2htdpuniverse.html?q=on-tick#(form._((lib._2htdp/universe..rkt)._on-tick))

on-tick!

on-key

Same as on-key:
http://docs.racket-lang.org/teachpack/2htdpuniverse.html?q=on-tick#(form._((lib._2htdp/universe..rkt)._on-key))

on-key!


on-click


on-click!

stop-when
stop-when!

to-draw
Same as single-argument to-draw:
http://docs.racket-lang.org/teachpack/2htdpuniverse.html?q=on-tick#(form._((lib._2htdp/universe..rkt)._to-draw))

to-draw-page
initial-effect

js-p
js-div
js-button
js-input
js-img
js-text
js-select

world-with-effects




Examples

