#lang scribble/manual

@(require (for-label "../image/image.rkt"))

@section{Images}
@defmodule[image]
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

@defproc[(overlay) image?]{}
@defproc[(overlay/xy) image?]{}
@defproc[(underlay) image?]{}
@defproc[(underlay/xy) image?]{}

@defproc[(put-pinhole) image?]{}

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