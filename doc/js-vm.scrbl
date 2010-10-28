#lang scribble/manual
@(require unstable/scribble)


@(require (for-label (planet dyoo/js-vm/image/image))
          (for-label (planet dyoo/js-vm/jsworld/jsworld))
          (for-label (planet dyoo/js-vm/phone/location))
          (for-label (planet dyoo/js-vm/phone/tilt))
          (for-label (planet dyoo/js-vm/phone/sms)))


@title{@bold{js-vm}: Javascript virtual machine for Racket}
@author[(author+email "Danny Yoo" "dyoo@racket-lang.org")]

This package provides tools to develop Racket programs that run
in Javascript.  It provides a Javascript runtime that interprets
Racket bytecode, functions for building and testing packages of
translated code, and libraries to use features of a web-browser's
environment.


At the moment, js-vm currently supports programs written in the
@schememodname/this-package[lang/wescheme] and
@schememodname/this-package[lang/base] languages; 
further development on @bold{js-vm} will work toward supporting
modules written in full Racket.

To install @bold{js-vm}, evaluate the following the DrRacket REPL.
@racketblock[(require #,(schememodname/this-package))]

This should install the library.


To make sure @bold{js-vm} is working, save the following program 
as test.rkt.
@racketmod[planet #,(this-package-version-symbol)
(printf "hello world\n")
(check-expect (* 3 4 5) 60)
(current-seconds)
(image-url "http://racket-lang.org/logo.png")
(check-expect (big-bang 0
                        (on-tick add1 1)
                        (stop-when (lambda (x) (= x 10))))
              10)
"last line"
]

This program uses a language that has been enriched with
Javascript-specific functions.  It can be partially evaluated in plain
Racket, but evaluation will halt at the call to @racket[image-url]
because @racket[image-url] constructs a image DOM element and needs to
run in an Javascript context.


Once the program is saved, create a new file called run.rkt with the
following:

@racketmod[racket
(require #,(this-package-version-symbol))
(run-in-browser "test.rkt")
]

When this program is executed, run-in-browser will take test.rkt and
translate it to run on the browser; a temporary web-server will opened
and your browser's window will open with the running program.


Finally, you can create zip packages by using @racket[create-zip-package].  For
example, modify run.rkt to be:

@racketmod[racket
(require #,(this-package-version-symbol))
#;(run-in-browser "test.rkt")
(create-zip-package "test.rkt" "test.zip")
]






@section{Base language}
@defmodule/this-package[lang/base]
This provides the basic set of bindings, including the bindings
from ASL.


@section{WeScheme}
@defmodule/this-package[lang/wescheme]
This provides the bindings from 
@schememodname/this-package[lang/base],
@schememodname/this-package[lang/posn],
@schememodname/this-package[image/image],
@schememodname/this-package[jsworld/jsworld], and
@schememodname/this-package[check-expect/check-expect].

It also adds @racket[open-image-url] and @racket[js-big-bang]
as aliases for @racket[image-url]
and @racket[big-bang] respectively.



@section{Jsworld}


jsworld provides a world programming library that allows simple
animation and games, as well as reactive HTML graphical user
interfaces.

@defmodule/this-package[jsworld/jsworld]


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


@defproc[(initial-effect [an-effect effect?]) handler?] {
Produces a handler that tells big-bang to apply an effect on
initialization.
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



@defproc[(stop-when!) handler?]{Produces a handler that stops a @racket[big-bang] whenever
the provided world function produces true.}                               









@defproc[(key=? [key1 key?] [key2 key?]) boolean?]{Produces true if @racket[key1] is equal to @racket[key2].}


@subsection{Event handlers}

@defproc[(on-tick) handler?]{Produces a handler that responds to clock ticks.}

@defproc[(on-tick!) handler?]{Produces a handler that responds to clock ticks.}

@defproc[(on-key) handler?]{Produces a handler that responds to key events.}

@defproc[(on-key!) handler?]{Produces a handler that responds to key events.}

@defproc[(on-button-click) handler?]{Produces a handler that responds to button click events.}

@defproc[(on-button-click!) handler?]{Produces a handler that responds to button click events.}



@subsection{HTML user interface constructors}


@defproc[(js-p) ...]{}
@defproc[(js-div) ...]{}
@defproc[(js-button) ...]{}
@defproc[(js-input) ...]{}
@defproc[(js-img) ...]{}
@defproc[(js-text) ...]{}
@defproc[(js-select) ...]{}



@subsection{Effects}




@subsection{Miscellaneous functions}

world-with-effects ...

Wraps a world with a collection of effects.








@section{Images}
@defmodule/this-package[image/image]
The contents of this module need to run in a Javascript context.

This module provides functions for creating images.  The design of the library is meant to
follow 2htdp/image.

@defproc[(image? [x any/c]) boolean?]{
Produces @racket[#t] if @racket[x] is an image, and @racket[#f] otherwise.}

@defproc[(image=? [x any/c] [y any/c]) boolean?]{
Produces @racket[#t] if @racket[x] is the same image as @racket[y].}


@defproc[(circle [radius nonnegative-real?] [style (one-of/c 'solid 'outline)] [color color?]) image?]{
Produces a circle image with the given radius, style, and color.
}


@defproc[(nw:rectangle [width number?] [height number?] [style (one-of/c 'solid 'outline)] [color color?]) image?]{Produces a rectangle whose pinhole is at the upper left corner.}


@defproc[(rectangle [width number?] [height number?] [style (one-of/c 'solid 'outline)] [color color?]) image?]{Produces a rectangle whose pinhole is at its center.}


@defproc[(triangle) image?]{Produces a triangle.}


@defproc[(ellipse) image?]{}


@defproc[(line) image?]{Creates a line.}


@defproc[(text [message string?] [size number?] [color color?]) image?]{Creates a text image.}


@defproc[(image-url [url string?]) image?]{Reads in the image at the provided url and produces an image of its contents.}


@defproc[(star) image?]{Creates a star image}




@defproc[(empty-scene [width number?] [height number?]) image?]{Produces an empty scene with a border.}

@defproc[(place-image [x number?] [y number?] [an-image image?] [background image?]) image?]{
Places @racket[an-image] on top of @racket[background] at the given
@racket[x], @racket[y] coordinate.}

@defproc[(overlay [img1 image?] [img2 image?] ...) image?]{}
@defproc[(overlay/xy [img1 image?] [x real?] [y real?] [img2 image?]) image?]{}
@defproc[(underlay [img1 image?] [img2 image?] ...) image?]{}
@defproc[(underlay/xy [img1 image?] [x real?] [y real?] [img2 image?]) image?]{}

@defproc[(put-pinhole [img image?] [x real?] [y real?]) image?]{}

@defproc[(image-width [an-image image?]) number?]{Produces the width of an image.}
@defproc[(image-height [an-image image?]) number?]{Produces the height of an image.}

Colors can be specified either by an RGB color structure, or by string name.  Both are described now.

@defproc[(make-color [red number?] [green number?] [blue number?]) color]{
Produces a color with the given RGB triplet.}

@defproc[(color-red [c color?]) number]{Selects the red part of the color.}
@defproc[(color-green [c color?]) number]{Selects the green part of the color.}
@defproc[(color-blue [c color?]) number]{Selects the blue part of the color.}

@subsection{Available colors}
Here is a complete list of the strings that @racket[image] will recognize as colors.
@(apply itemlist
        (map (lambda (str) (item (racket #,(string-downcase str))))
             '("ORANGE"
               "RED"
               "ORANGERED"
               "TOMATO"
               "DARKRED"
               "RED"
               "FIREBRICK"
               "CRIMSON"
               "DEEPPINK"
               "MAROON"
               "INDIAN RED"
               "INDIANRED"
               "MEDIUM VIOLET RED"
               "MEDIUMVIOLETRED"
               "VIOLET RED"
               "VIOLETRED"
               "LIGHTCORAL"
               "HOTPINK"
               "PALEVIOLETRED"
               "LIGHTPINK"
               "ROSYBROWN"
               "PINK"
               "ORCHID"
               "LAVENDERBLUSH"
               "SNOW"
               "CHOCOLATE"
               "SADDLEBROWN"
               "BROWN"
               "DARKORANGE"
               "CORAL"
               "SIENNA"
               "ORANGE"
               "SALMON"
               "PERU"
               "DARKGOLDENROD"
               "GOLDENROD"
               "SANDYBROWN"
               "LIGHTSALMON"
               "DARKSALMON"
               "GOLD"
               "YELLOW"
               "OLIVE"
               "BURLYWOOD"
               "TAN"
               "NAVAJOWHITE"
               "PEACHPUFF"
               "KHAKI"
               "DARKKHAKI"
               "MOCCASIN"
               "WHEAT"
               "BISQUE"
               "PALEGOLDENROD"
               "BLANCHEDALMOND"
               "MEDIUM GOLDENROD"
               "MEDIUMGOLDENROD"
               "PAPAYAWHIP"
               "MISTYROSE"
               "LEMONCHIFFON"
               "ANTIQUEWHITE"
               "CORNSILK"
               "LIGHTGOLDENRODYELLOW"
               "OLDLACE"
               "LINEN"
               "LIGHTYELLOW"
               "SEASHELL"
               "BEIGE"
               "FLORALWHITE"
               "IVORY"
               "GREEN"
               "LAWNGREEN"
               "CHARTREUSE"
               "GREEN YELLOW"
               "GREENYELLOW"
               "YELLOW GREEN"
               "YELLOWGREEN"
               "MEDIUM FOREST GREEN"
               "OLIVEDRAB"
               "MEDIUMFORESTGREEN"
               "DARK OLIVE GREEN"
               "DARKOLIVEGREEN"
               "DARKSEAGREEN"
               "LIME"
               "DARK GREEN"
               "DARKGREEN"
               "LIME GREEN"
               "LIMEGREEN"
               "FOREST GREEN"
               "FORESTGREEN"
               "SPRING GREEN"
               "SPRINGGREEN"
               "MEDIUM SPRING GREEN"
               "MEDIUMSPRINGGREEN"
               "SEA GREEN"
               "SEAGREEN"
               "MEDIUM SEA GREEN"
               "MEDIUMSEAGREEN"
               "AQUAMARINE"
               "LIGHTGREEN"
               "PALE GREEN"
               "PALEGREEN"
               "MEDIUM AQUAMARINE"
               "MEDIUMAQUAMARINE"
               "TURQUOISE"
               "LIGHTSEAGREEN"
               "MEDIUM TURQUOISE"
               "MEDIUMTURQUOISE"
               "HONEYDEW"
               "MINTCREAM"
               "ROYALBLUE"
               "DODGERBLUE"
               "DEEPSKYBLUE"
               "CORNFLOWERBLUE"
               "STEEL BLUE"
               "STEELBLUE"
               "LIGHTSKYBLUE"
               "DARK TURQUOISE"
               "DARKTURQUOISE"
               "CYAN"
               "AQUA"
               "DARKCYAN"
               "TEAL"
               "SKY BLUE"
               "SKYBLUE"
               "CADET BLUE"
               "CADETBLUE"
               "DARK SLATE GRAY"
               "DARKSLATEGRAY"
               "LIGHTSLATEGRAY"
               "SLATEGRAY"
               "LIGHT STEEL BLUE"
               "LIGHTSTEELBLUE"
               "LIGHT BLUE"
               "LIGHTBLUE"
               "POWDERBLUE"
               "PALETURQUOISE"
               "LIGHTCYAN"
               "ALICEBLUE"
               "AZURE"
               "MEDIUM BLUE"
               "MEDIUMBLUE"
               "DARKBLUE"
               "MIDNIGHT BLUE"
               "MIDNIGHTBLUE"
               "NAVY"
               "BLUE"
               "INDIGO"
               "BLUE VIOLET"
               "BLUEVIOLET"
               "MEDIUM SLATE BLUE"
               "MEDIUMSLATEBLUE"
               "SLATE BLUE"
               "SLATEBLUE"
               "PURPLE"
               "DARK SLATE BLUE"
               "DARKSLATEBLUE"
               "DARKVIOLET"
               "DARK ORCHID"
               "DARKORCHID"
               "MEDIUMPURPLE"
               "CORNFLOWER BLUE"
               "MEDIUM ORCHID"
               "MEDIUMORCHID"
               "MAGENTA"
               "FUCHSIA"
               "DARKMAGENTA"
               "VIOLET"
               "PLUM"
               "LAVENDER"
               "THISTLE"
               "GHOSTWHITE"
               "WHITE"
               "WHITESMOKE"
               "GAINSBORO"
               "LIGHT GRAY"
               "LIGHTGRAY"
               "SILVER"
               "GRAY"
               "DARK GRAY"
               "DARKGRAY"
               "DIM GRAY"
               "DIMGRAY"
               "BLACK")))




@section{Phone}


location.rkt
    on-location-change: world-updater  -> handler
    on-location-change!: world-updater effect-f -> handler 


sms.rkt

    on-sms-receive: world-updater -> handler
    on-sms-receive!: world-updater effect-f -> handler


tilt.rkt

    on-acceleration: (world number number number -> world)
    on-acceleration!: ...
    on-shake
    on-shake!
    on-tilt
    on-tilt!








@section{Implementing Javascript Modules}
Warning: the material in this section is unstable, 
woefully underdocumented, and likely to change.

@subsection{Module Implementation in Javascript}
@defmodule/this-package[lang/js-impl/js-impl]
This module allows the definition of modules whose implementations
are written in Javascript.

As an example, the following two files provide an implementation
of @racket[double] in Javascript:
@racketmod[planet #,(this-package-version-symbol js-impl)
(require-js "double.js")
(provide double)]

@verbatim{
// double.js
EXPORTS['double'] = 
    new types.PrimProc('double', 1, false, false, function(x) { 
        return jsnums.multiply(x, 2)});
}

Any module implemented with 
@schememodname/this-package[lang/js-impl/js-impl]
will provide bindings that require a Javascript context.



@subsection{Conditional Module Implmentation in javascript}
@defmodule/this-package[lang/js-impl/js-conditional]


Any module implemented with 
@schememodname/this-package[lang/js-conditional/js-conditional]
can run either in a Racket or Javascript context.


@section{Foreign Function Interface}
@defmodule/this-package[ffi/ffi]
The contents of this module need to run in a Javascript context.
