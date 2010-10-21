#lang scribble/manual
@(require unstable/scribble)
@(require (for-label (planet dyoo/js-vm:1:3/image/image)))


@title{@bold{js-vm}: Javascript virtual machine for Racket}
@author[(author+email "Danny Yoo" "dyoo@racket-lang.org")]

This package provides tools to develop Racket programs that run
in Javascript.  It provides a Javascript runtime that interprets
Racket bytecode, a DrRacket tool for building and testing packages of
translated code, and libraries to use features of a web-browser's
environment.


At the moment, js-vm currently supports programs written in the
@schememodname/this-package[lang/wescheme] and
@schememodname/this-package[lang/base] languages; 
further development on @bold{js-vm} will work toward supporting
modules written in full Racket.

To install @bold{js-vm},
@racketblock[(require #,(schememodname/this-package info))]

This  should install the library and its associated DrRacket tool;
the tool adds two menu options:
@itemize[@item{Create Javascript Package} 
          @item{Run Javascript in Browser}]
Restart DrRacket to make sure the tool is loaded.


To make sure @bold{js-vm} is working, try to run the following program in
DrRacket.
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
Evaluation should halt at the call to @racket[image-url], because
@racket[image-url] constructs a image DOM element and
needs to run in an Javascript context.

Save the program, and then select @emph{Run Javascript in Browser} 
under the @emph{Racket} submenu.

Finally, you can construct a .zip package of a program by
selecting @emph{Create Javascript Package} under the @emph{Racket} submenu.





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

It also adds open-image-url and js-big-bang as aliases for image-url
and big-bang respectively.


@section{Foreign Function Interface}
@defmodule/this-package[ffi/ffi]
The contents of this module need to run in a Javascript context.



@section{Images}
@defmodule/this-package[image/image]
The contents of this module need to run in a Javascript context.

This module provides functions for creating images.  The design of the library is meant to
follow 2htdp/image.


@section{Image constructors}
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




@section{Image combinators}
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


@section{Colors}

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