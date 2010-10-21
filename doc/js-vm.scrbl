#lang scribble/manual
@(require unstable/scribble)


@title{@bold{js-vm}: Javascript virtual machine for Racket}
@author[(author+email "Danny Yoo" "dyoo@racket-lang.org")]

This package provides tools to develop Racket programs that run
within Javascript.  It provides a Javascript runtime that interprets
Racket bytecode, a DrRacket tool for building and testing packages of
translated code, and libraries that take advantage of the browser
environment.


@section{Introduction}




@section{Libraries}

@subsection{WeScheme}
@defmodule/this-package[lang/wescheme]

@subsection{Foreign Function Interface}
@defmodule/this-package[ffi/ffi]

@subsection{Images}
@defmodule/this-package[image/image]


@subsection{Module Implementation in Javascript}
@defmodule/this-package[lang/js-impl/js-impl]

@subsection{Conditional Module Implmentation in javascript}
@defmodule/this-package[lang/js-impl/js-conditional]
