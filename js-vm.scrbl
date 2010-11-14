#lang scribble/manual
@(require unstable/scribble)


@(require (for-label (planet dyoo/js-vm/image/image))
          (for-label (planet dyoo/js-vm/jsworld/jsworld))
          (for-label (planet dyoo/js-vm/main)))

@(define (js-vm)
   (emph "js-vm"))


@title{@js-vm[]: Javascript virtual machine for Racket}
@author[(author+email "Danny Yoo" "dyoo@racket-lang.org")]



@js-vm[] provides tools to develop Racket programs that run
in Javascript.  It provides a Javascript runtime that interprets
Racket bytecode, functions for building and testing packages of
translated code, and libraries to use features of a web-browser's
environment.

This project is intimately related with @hyperlink["http://www.cs.brown.edu/~sk/Publications/Talks/Moby-Bootstrap/"]{Moby}, as Moby uses @js-vm[] as its underlying runtime.
       

@section{Quick Start}

To make sure @js-vm[] is working, save the following program 
as @filepath{test.rkt} in some working directory.
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

This program is in a language that has been enriched with
Javascript-specific functions.  It can be partially evaluated in plain
Racket, but evaluation will halt at the call to @racket[image-url]
because @racket[image-url] constructs a image DOM element and needs to
run in an Javascript context.


Once the program is saved, create a new file called @filepath{run.rkt} in the same working directory with the
following contents:

@racketmod[racket
           (require #,(schememodname/this-package))
           (run-in-browser "test.rkt")
]

When this program is executed, @racket[run-in-browser] will take @filepath{test.rkt} and
translate it to run on the browser; a temporary web-server will opened
and your browser's window will open with the running program.


Finally, you can create zip packages by using @racket[create-zip-package].  For
example, modify @filepath{run.rkt} to be:

@racketmod[racket
           (require #,(this-package-version-symbol))
           (create-zip-package "test.rkt" "test.zip")
]




A slightly more substantial example is an animation using a built-in
functional event-driven programming library.


@racketmod[planet #,(this-package-version-symbol)
           @code:comment{falling.ball.rkt}
           @code:comment{Simple falling ball example.  A red ball falls down the screen}
           @code:comment{until hitting the bottom.}
           (define-struct world (radius y))
           
           @code:comment{The dimensions of the screen:}
           (define WIDTH 320)
           (define HEIGHT 480)
           
           @code:comment{The radius of the red circle.}
           (define RADIUS 15)
           
           @code:comment{The world is the distance from the top of the screen.}
           (define INITIAL-WORLD (make-world RADIUS 0))
           
           @code:comment{tick: world -> world}
           @code:comment{Moves the ball down.}
           (define (tick w)
             (make-world RADIUS (+ (world-y w) 5)))
           
           
           @code:comment{hits-floor?: world -> boolean}
           @code:comment{Returns true when the distance reaches the screen height.}
           (define (hits-floor? w)
             (>= (world-y w) HEIGHT))
           
           @code:comment{We have some simple test cases.}
           (check-expect (hits-floor? (make-world RADIUS 0)) false)
           (check-expect (hits-floor? (make-world RADIUS HEIGHT)) true)
           
           @code:comment{render: world -> scene}
           @code:comment{Produces a scene with the circle at a height described by the world.}
           (define (render w)
             (place-image (circle RADIUS "solid" "red")
                          (/ WIDTH 2) 
                          (world-y w)
                          (empty-scene WIDTH HEIGHT)))
           
           @code:comment{Start up a big bang, 15 frames a second.}
           (check-expect (big-bang INITIAL-WORLD
                                   (on-tick tick 1/15)
                                   (to-draw render)
                                   (stop-when hits-floor?))
                         (make-world 15 480))
                                             
]

Again, to run this in the browser, we use @racket[run-in-browser]:
@racketmod[racket
           (require #,(schememodname/this-package))
           (run-in-browser "falling-ball.rkt")
]




@section{Running @js-vm[]}
@defmodule/this-package[]
@defproc[(run-in-browser [input-file path-string?]) void]{
Consumes the given program, translates it so it can run on the browser,
and brings up the default browser.

At the moment, @js-vm[] currently supports programs written in the
@schememodname/this-package[lang/wescheme] and
@schememodname/this-package[lang/base] languages; further development
on @js-vm[] will work toward supporting modules written in full
Racket.  @racket[require] should work as long as the required modules,
too, are in the supported languages.

}

@defproc[(create-zip-package [input-file path-string?] [output-zip-file path-string?]) void]{
Consumes the given program, translates it so it can run on the browser,
and writes out a zip archive to the given path.  If the output file already exists, overwrites it.  This zip file can be unpacked and served on any standard
web server.
}








@section{WeScheme}
@defmodule/this-package[lang/wescheme]
The language here acts as a kind of ``Pretty Big'' language,
and is the language used when @racket[planet #,(this-package-version-symbol)] is
the module language.

It provides the bindings from 
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


@defproc[(big-bang (a-world world) (handlers handler) ...) world]{
Starts a reactive computation with @racket[big-bang].
The rest of the arguments hook into the reactive computation.

By default, the page that's displayed contains a rendering of the
world value.  In the presence of an @racket[to-draw] or
@racket[to-draw-page] handler, @racket[big-bang] will show a
customized view.

The majority of the handlers register different stimuli that can
trigger changes to the world.  One instance is @racket[on-tick], which
registers a function to update the world on a clock tick.  }

When the @racket[big-bang] computation terminates through a
@racket[stop-when], the final world is returned as its value.









                                                                                                                              
                                                                                                                 
@defproc[(to-draw [hook (world -> scene)]) handler]{

Draws a scene onto the display.  For simple applications,
@racket[to-draw] is sufficient to draw a scene onto the display.}


@defproc[(stop-when [stop? (world -> boolean)]) handler?]{
When the world should be stopped --- when @racket[stop?] applied to the world
produces @racket[true] --- then the @racket[big-bang] terminates.

The program:
@racketmod[planet #,(this-package-version-symbol)
           (define (at-ten x)
             (>= x 10))
           
           (big-bang 0
                     (on-tick add1 1)
                     (stop-when at-ten))
                                        ]
counts up to ten and then stops.
}


@defproc[(on-tick [world-updater (world -> world)]
                  [delay number? 1/20]) handler?]{
  Produces a handler that responds to clock ticks.  By default,
  every tick comes every @racket[1/20]'th of a second.}

@defproc[(on-key [world-updater (world key? -> world)]) handler?]{
Produces a handler that responds to key events.}

@defproc[(key=? [key1 key?] [key2 key?]) boolean?]{Produces true if @racket[key1] is equal to @racket[key2].}



@; As soon as we have this, we'll comment it.
@;
@;{@defproc[(on-button-click) handler?]{Produces a handler that responds to button click events.}}





  
  
  

@defproc[(to-draw-page [to-dom (world -> (DOM-sexp))]
		       [to-css (world -> (CSS-sexp))]) handler]{

One of the main handlers to @racket[big-bang] is @racket[to-draw-page],
which controls how the world is rendered on screen.  The first
argument computes a rendering of the world as a DOM tree, and the
second argument computes that tree's styling.  }


@subsection{Jsworld Types}

A @racket[dom-sexp] describes the structure of a web page:

@racketgrammar[dom-sexp (list dom-element dom-sexp ...)]


a @racket[css-sexp] describes the structure of a page's styling:

@racketgrammar[css-sexp (listof (cons (or dom-element string)
                                      (listof attrib)))]

An @racket[attrib] is a:
@racketgrammar[attrib (list string string)]

Each of the @racket[dom-element]s can take in an optional attribute list to
assign to the new dom element; the common useful attribute is a key-value binding for an "id",
which can be used to identify an element in the css-drawing function.


Here are examples  of a dom-expr and a css-sexp.
@racketblock[
             (define a-dom-sexp (list (js-div '(("id" "main-div")))
                                      (list (js-text "Hello world"))))
             
             (define a-css-sexp (list (list "main-div"
                                            (list "background" "white")
                                            (list "font-size" "40px"))))
                                                                        ]
                                                               

  
  
@subsection{HTML user interface constructors}










Here are the dom-element constructors.

@defproc[(js-div (attribs (listof attrib?) '())) dom-element?]{
Constructs a div element.}


@defproc[(js-p (attribs (listof attrib?) '())) dom-element?]{
Constructs a paragraph element.}

@defproc[(js-button (world-update-f (world -> world)) 
                    (attribs (listof attrib) '()))
         dom-element]{
Constructs a button.  When the button is pressed, the world is updated through @racket[world-update-f].

The following example counts how many times a button has been clicked.
@(racketmod planet #,(this-package-version-symbol)
            (define (press w)
              (add1 w))

            (define (draw w)
              (list (js-div)
                    (list (js-button press) (list (js-text "Press me")))
                    (list (js-text (format "Button presses: ~a" w)))))
            
            (define (draw-css w)
              '())
            
            (big-bang 0
                      (to-draw-page draw draw-css)))
}



@; commenting out effectful button.
@;{
@defproc[(js-button! (world-update-f (world -> world))
                     (effect-f (world -> effect))
                     (attribs (listof attrib) '()))
         dom-element]{
Constructs a button.  When the button is pressed, the original world is updated,
and the original world is used to construct an effect.
}}


                     
                     
                   
        
@defproc[(js-text (text string?)) dom-element]{Constructs regular text.}

@defproc[(js-input (type string)
                   (world-update-f (or/c (world string -> world)
                                         (world boolean -> world)))
                   (attribs (listof attrib) '()))
         dom-element]{
Creates an input form element.  The types that are currently supported are:
@itemlist[@item{@racket["text"]}
          @item{@racket["password"]}
          @item{@racket["checkbox"]}]
When the user changes the content of the form element, the runtime
uses @racket[world-update-f] to update the world.  If the
@racket[type] is either @racket["text"] or @racket["password"], then
the string value of the element will be passed as the second argument
to it.  If @racket[type] is @racket["checkbox"], a boolean
representing the checked status of the element will be passed to it.

The example below has a single text input form element, which allows the user to enter
some value.
@(racketmod planet #,(this-package-version-symbol)
            (define (refresh w form-val)
              form-val)
            
            (define input-node
              (js-input "text" refresh '(("id" "myname"))))
            
            (define (draw w)
              (list (js-div)
                    (list (js-div) (list (js-text (format "I see: ~s~n" w))))
                    (list (js-div) (list input-node))))
            
            
            (define (draw-css w)
              '())
            
            (big-bang ""
                      (to-draw-page draw draw-css))
            )


The example below uses a checkbox to select among three elements:
@(racketmod planet #,(this-package-version-symbol)
            (define (make-ingredient-checkbox-sexp ingredient)
              (local [(define (on-check w v)
                        (cond
                          [v
                           (cons ingredient w)]
                          [else
                           (remove ingredient w)]))]
                (list (js-div)
                      (list (js-text ingredient))
                      (list (js-input "checkbox" 
                                      on-check
                                      `(("value" ,ingredient)))))))
            
            (define c1 (make-ingredient-checkbox-sexp "mushrooms"))
            (define c2 (make-ingredient-checkbox-sexp "green peppers"))
            (define c3 (make-ingredient-checkbox-sexp "olives"))
            
            (define (draw w)
              (list (js-div)
                    c1
                    c2
                    c3
                    (list (js-text (format "The world is: ~s" w)))))
            
            (define (draw-css w)
              '())
            
            (big-bang '()
                      (to-draw-page draw draw-css)))
}





@defproc[(js-img (url string) (attribs (listof attrib) '())) dom-element]{Creates an image element.
}

@defproc[(js-select (options (listof string?)) (world-update-f (world string -> world)) (attribs (listof attrib) '())) dom-element]
Constructs a select element with the given options.  Whenever a new
option is selected, the @racket[world-update-f] function is called to
get the new world.

The example below has a select with five elements.
@(racketmod planet #,(this-package-version-symbol)
            (define (select-house w an-option)
              an-option)
            
            (define a-select-element
              (js-select (list ""
                               "Gryffindor"
                               "Hufflepuff" 
                               "Ravenclaw"
                               "Slytherin")
                         select-house))
            
            (define (draw w)
              (list (js-div)
                    (list a-select-element)
                    (list (js-text (format "House: ~a" w)))))
            
            (define (draw-css w)
              '())
            
            (big-bang ""
                      (to-draw-page draw draw-css))
            )

  
  

@;; Effects section is currently commented out: we did not 
@;; yet port this over from the old Moby system.

@;{   
@subsection{Effects}

Effects allow world programs to apply side effects to the outside
world.  These are used in conjunction with the effect (@racket[!]) version of the
stimulus handlers described above.


@defproc[(make-effect:none) effect]{No result when interpreted.}
@defproc[(make-effect:beep) effect]{Audible beep when interpreted.  On an Android smartphone, uses the notification ringtone.}


@defproc[(make-effect:play-sound (a-sound sound)) effect]{Plays a sound from the given @racket[sound].  If the sound is already playing, then the sound continues to play.}
@defproc[(make-effect:stop-sound (a-sound sound)) effect]{Stops playing a sound from the given url.}
@defproc[(make-effect:pause-sound (a-sound sound)) effect]{Pauses a sound; if @racket[make-effect:play-sound] for the same sound is given later, play restarts from the point where it was paused.}

A @racket[sound] is a:
@racketgrammar[sound string
                     playlist]


@defproc[(make-effect:set-sound-volume (volume number)) effect]{Sets the sound volume; the number should be between 0 and 100.}
@defproc[(make-effect:raise-sound-volume) effect]{Raises the sound volume.  If the volume's already at 100, has no effect.}
@defproc[(make-effect:lower-sound-volume) effect]{Lowers the sound volume.  If the volume's set to 0, has no effect.}

@defproc[(make-effect:set-beep-volume (volume number)) effect]{Sets the sound volume of the beep; the number should be between 0 and 100.  On an Android smartphone, uses the notification sound.}

@defproc[(make-effect:play-dtmf-tone (tone number)) effect]{On a smartphone, plays a DTMF tone, where @racket[tone] is between 0 and 15 inclusive.}

@defproc[(make-effect:set-wake-lock (flag number)) effect]{On a smartphone, sets the wake-lock flag to prevent the phone from sleeping.  Low-level call.}
@defproc[(make-effect:release-wake-lock) effect]{On a smartphone, releases a wake-lock to allow the phone to go to sleep again.}

@defproc[(make-effect:send-sms (phone-number string) (message string)) effect]{Sends an SMS message.}

@; Commenting out the generate-random documentation
@;{@defproc[(make-effect:generate-random: (world-update-f (world number -> world))) effect]{When interpreted, generates a random number on the fly and uses @racket[world-update-f] to update the world.}}

@defproc[(make-effect:pick-playlist (world-update-f (world playlist -> world))) effect]{Brings up a playlist picker; when a playlist is selected, the world is updated using @racket[world-update-f] with the selected @racket[playlist] sound.}
   }
  






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


@defproc[(rotate [degree real?] [img image?]) image?]{Rotates the given by the degree.}


@defproc[(scale [factor real?] [img image?]) image?]{Scales the image.}


@defproc[(scale/xy [x-factor real?] [y-factor real?] [img image?]) image?]{Scales the image by the given @racket[x-factor], @racket[y-factor].}


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






@section{Implementing Javascript Modules}
Warning: the material in this section is unstable and likely to change.

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


@include-section["ffi/ffi.scrbl"]









@section{Base language}
@defmodule/this-package[lang/base]
This provides the basic set of bindings for @js-vm[] programs.
These include most of the bindings
from @hyperlink["http://docs.racket-lang.org/htdp-langs/advanced.html"]{ASL}
and some from regular Racket, including:
@(let ([names '(
		;#%module-begin
		;#%datum
		;#%app
		;#%top-interaction
		;#%top
		define
		define-struct
		if
		cond
		else
		case
		quote
		unquote
		unquote-splicing
		lambda
		case-lambda
		let
		let*
		letrec
		letrec-values
		local
		quasiquote
		begin
		begin0
		set!
		and
		or
		when
		unless
		recur
		require
		for-syntax
		define-for-syntax
		begin-for-syntax
		prefix-in
		only-in
		provide
		planet
		all-defined-out
		all-from-out
		except-out
		rename-out    
		define-syntax
		let/cc
		with-continuation-mark
		
		true
		false
		pi
		e
		empty
		eof
		null


		shared 
                with-handlers

		write
		display
		newline
		current-print
		current-continuation-marks
		continuation-mark-set?
		continuation-mark-set->list
		for-each
		;; make-thread-cell
		make-struct-type
		make-struct-field-accessor
		make-struct-field-mutator
		struct-type?
		struct-constructor-procedure?
		struct-predicate-procedure?
		struct-accessor-procedure?
		struct-mutator-procedure?
		procedure-arity
		procedure-arity-includes?
		make-arity-at-least
		arity-at-least?
		arity-at-least-value
		apply
		values
		call-with-values
		compose
		current-inexact-milliseconds
		current-seconds
		not
		void
		random
		sleep
		identity
		raise
		error

		make-exn
		make-exn:fail
		make-exn:fail:contract
		make-exn:fail:contract:arity
		make-exn:fail:contract:variable
		make-exn:fail:contract:divide-by-zero

		exn-message
		exn-continuation-marks

		exn?
		exn:fail?
		exn:fail:contract?
		exn:fail:contract:arity?
		exn:fail:contract:variable?
		exn:fail:contract:divide-by-zero?


		*
		-
		+
		=
		=~
		/
		sub1
		add1
		<
		>
		<=
		>=
		abs
		quotient
		remainder
		modulo
		max
		min
		gcd
		lcm
		floor
		ceiling
		round
		numerator
		denominator
		expt
		exp
		log
		sin
		cos
		tan
		asin
		acos
		atan
		sinh
		cosh
		sqr
		sqrt
		integer-sqrt
		make-rectangular
		make-polar
		real-part
		imag-part
		angle
		magnitude
		conjugate
		sgn
		inexact->exact
		exact->inexact
		number->string
		string->number
		procedure?
		pair?
		cons?
		empty?
		null?
		undefined?
		immutable?
		void?
		symbol?
		string?
		char?
		boolean?
		vector?
		struct?
		eof-object?
		bytes?
		byte?
		number?
		complex?
		real?
		rational?
		integer?
		exact?
		inexact?
		odd?
		even?
		zero?
		positive?
		negative?
		box?
		hash?
		eq?
		eqv?
		equal?
		equal~?
		false?
		boolean=?
		symbol=?
		cons
		car
		cdr
		caar
		cadr
		cdar
		cddr
		caaar
		caadr
		cadar
		cdaar
		cdadr
		cddar
		caddr
		cdddr
		cadddr
		rest
		first
		second
		third
		fourth
		fifth
		sixth
		seventh
		eighth
		length
		list?
		list
		list*
		list-ref
		list-tail
		append
		reverse
		map
		andmap
		ormap
		memq
		memv
		member
		memf
		assq
		assv
		assoc
		remove
		filter
		foldl
		foldr
		quicksort
		sort
		argmax
		argmin
		build-list
		box
		box-immutable
		unbox
		set-box!
		make-hash
		make-hasheq
		hash-set!
		hash-ref
		hash-remove!
		hash-map
		hash-for-each
		make-string
		replicate
		string
		string-length
		string-ref
		string=?
		string-ci=?
		string<?
		string>?
		string<=?
		string>=?
		string-ci<?
		string-ci>?
		string-ci<=?
		string-ci>=?
		substring
		string-append
		string->list
		list->string
		string-copy
		string->symbol
		symbol->string
		format
		printf
		string->int
		int->string
		explode
		implode
		string-alphabetic?
		string-ith 
		string-lower-case?
		string-numeric?
		string-upper-case?
		string-whitespace?
		build-string
		string->immutable-string
		string-set!
		string-fill!
		make-bytes
		bytes
		bytes->immutable-bytes
		bytes-length
		bytes-ref
		bytes-set!
		subbytes
		bytes-copy
		bytes-fill!
		bytes-append
		bytes->list
		list->bytes
		bytes=?
		bytes<?
		bytes>?
		make-vector
		vector
		vector-length
		vector-ref
		vector-set!
		vector->list
		list->vector
		build-vector
		char=?
		char<?
		char>?
		char<=?
		char>=?
		char-ci=?
		char-ci<?
		char-ci>?
		char-ci<=?
		char-ci>=?
		char-alphabetic?
		char-numeric?
		char-whitespace?
		char-upper-case?
		char-lower-case?
		char->integer
		integer->char
		char-upcase
		char-downcase

		
		call-with-current-continuation
		call/cc
		call-with-continuation-prompt
		abort-current-continuation
		default-continuation-prompt-tag
		make-continuation-prompt-tag
		continuation-prompt-tag?


		make-reader-graph
		make-placeholder
		placeholder-set!
)])
   (apply itemize (map (lambda (i) (item (racket #,i))) names)))


@subsection{Deviations from ASL}

@itemize[
@item{@racket[+], @racket[*], @racket[/] don't take at least two arguments.}

@item{@racket[and], @racket[or] don't require at least two arguments.}

@item{@racket[set!] allowed on function arguments.}

@item{syntactic keywords can be used as variable names.}

@item{displayed output is not identical with regards to shared values
and constructor output.}


@item{The following primitives have not been implemented:
@itemize[
    @item{@racket[:] (signature definition)}
    @item{@racket[define-datatype]}
    @item{@racket[match]}
    @item{@racket[delay]}
    @item{@racket[check-member-of]}
    @item{@racket[check-range]}
    @item{@racket[check-error]}
    @item{@racket[exit]}
    @item{@racket[force]}
    @item{@racket[gensym]}
    @item{@racket[promise]}
    @item{@racket[pretty-print]}
    @item{@racket[print]}
    @item{@racket[read]}
    @item{@racket[with-input-from-file]}
    @item{@racket[with-input-from-string]}
    @item{@racket[with-output-to-file]}
    @item{@racket[with-output-to-string]}
    @item{@racket[hash-copy]}
    @item{@racket[hash-count]}
    @item{@racket[hash-eq?]}
    @item{@racket[hash-equal?]}
    @item{@racket[hash-eqv?]}
    @item{@racket[hash-has-key?]}
    @item{@racket[hash-ref!]}
    @item{@racket[hash-update!]}
    @item{@racket[make-hasheqv]}
    @item{@racket[make-immutable-hash]}
    @item{@racket[make-immutable-hasheq]}
    @item{@racket[make-immutable-hasheqv]}
]
}

]