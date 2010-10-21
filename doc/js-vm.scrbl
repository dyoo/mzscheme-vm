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

@racketmod[racket/base
(printf "hello world\n")]



@section{Libraries}

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
