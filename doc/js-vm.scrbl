#lang scribble/manual
@(require unstable/scribble)


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
Evaluation should halt at the call to @racket[image-url]; @racket[image-url]
constructs a image DOM element, but needs to run in an Javascript context.

Save the program, and then select @emph{Run Javascript in Browser} 
under the @emph{Racket} submenu.

Finally, you can construct a .zip package of a program by
selecting @emph{Create Javascript Package} under the @emph{Racket} submenu.


@section{Libraries}


@subsection{Base}
@defmodule/this-package[lang/base]
This provides the basic set of bindings; it includes the majority of
bindings from ASL.


@subsection{WeScheme}
@defmodule/this-package[lang/wescheme]
This provides the bindings from 
@schememodname/this-package[lang/base],
@schememodname/this-package[lang/posn],
@schememodname/this-package[image/image],
@schememodname/this-package[jsworld/jsworld], and
@schememodname/this-package[check-expect/check-expect].

It also adds open-image-url and js-big-bang as aliases for image-url
and big-bang respectively.


@subsection{Foreign Function Interface}
@defmodule/this-package[ffi/ffi]

@subsection{Images}
@defmodule/this-package[image/image]


@subsection{Module Implementation in Javascript}
@defmodule/this-package[lang/js-impl/js-impl]

@subsection{Conditional Module Implmentation in javascript}
@defmodule/this-package[lang/js-impl/js-conditional]
