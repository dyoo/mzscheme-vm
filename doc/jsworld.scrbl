#lang scribble/manual

@section{The Moby World API}
  

@defproc[(js-big-bang (a-world world) (handlers handler?) ...) void]{
A Moby program starts a reactive computation with @scheme[js-big-bang].
The rest of the arguments hook into the reactive computation.

By default, the page that's displayed contains a rendering of the world value.
In the presence of an @scheme[on-draw] or @scheme[on-redraw] handler, @scheme[js-big-bang] will
show a customized view.

The majority of the handlers register different stimuli that can trigger changes
to the world.  One
instance is @scheme[on-tick], which registers a function to update
the world on a clock tick.
}



@defproc[(on-draw [to-dom (world -> (DOM-sexp))]
                  [to-css (world -> (CSS-sexp))]) scene]{
One of the main handlers to @scheme[js-big-bang] is @scheme[on-draw], which controls how
the world is rendered on screen.  The first argument computes a
rendering of the world as a DOM tree, and the second argument computes
that tree's styling.
}


                                                                                                                 
@defproc[(on-redraw [hook (world -> scene)]) handler?]{
For simple applications, @scheme[on-redraw] is sufficient to draw a scene onto the display.
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

(js-big-bang INITIAL-WORLD
             (on-tick 1/15 tick)
             (on-redraw render)
             (stop-when hits-floor?)))
}


@defproc[(stop-when [stop? (world -> boolean)]) handler?]{
When the world should be stopped --- when @scheme[stop?] applied to the world
produces @scheme[true] --- then the @scheme[js-big-bang] terminates.

The program:
@(schemeblock
(define (at-ten x)
  (>= x 10))

(js-big-bang 0
             (on-tick 1 add1)
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

