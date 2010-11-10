#lang scribble/manual

@(require (for-label (planet dyoo/js-vm/ffi/ffi))
          unstable/scribble)



@title[#:tag "ffi"]{Racket to Javascript FFI}

@defmodule/this-package[ffi/ffi]

This API provides a foriegn function interface that allows javascript to be
accessed directly and explicitly through @racketmodname[racket]. This allows
access to native javascript functionality and allows external javascript APIs
to be used in @racketmodname[racket] code without modifying the source code of
the runtime infrastructure that evaluates the code.


The contents of this module need to run in a Javascript context.

@;-----------------------------------------------------------------------------

@section{Type Conversions}


These procedures are designed to convert between @racketmodname[racket] and
javascript types.

@defproc[(racket->prim-js
          [v (or/c boolean? char? real? string? symbol? vector?)])
         js-value?]{

Converts a @racketmodname[racket] value to a javascript value using the
following rules:

@itemize[

 @item{When @racket[v] is a boolean, string, or inexact number,
  @racket[racket->prim-js] converts directly to the javascript boolean, string,
  or number with the same value.}
  
 @item{When @racket[v] is an exact number, @racket[racket->prim-js] returns a
  floating point number equivalent to @racket[(racket->prim-js (exact->inexact
  v))] if @racket[v] is in [@racket[minimum-js-fixnum], @racket[maximum-js-fixnum]]. Otherwise it will raise an
  @racket[exn:fail:contract] exception.}
   
 @item{When @racket[v] is a char, @racket[racket->prim-js] returns a
  on-character javascript string corresponding to
  @racket[(racket->prim-js (string v))].}
 
 @item{When @racket[v] is a symbol, @racket[racket->prim-js] returns a
  javascript string corresponding to @racket[(racket->prim-js (symbol->string
  v))].}
  
 @item{When @racket[v] is a vector, @racket[racket->prim-js] returns a
  javascript array whose elements are @racket[eq?] to the elements of the
  vector.}

]}


The @racket[racket->prim-js] procedure cannot convert a procedure because
@racketmodname[racket] procedures cannot be converted to javascript in a
straightforward manner. When @racketmodname[racket] procedures are evaluated,
they are evaluated in continuation passing style, and hence any javascript
procedure constructed from a @racketmodname[racket] procedure must either be in
CPS or must return void.


@defproc[(procedure->cps-js-fun [proc procedure?]) js-function?]{

Converts a @racketmodname[racket] procedure to a javascript function. The
javascript function returned is in continuation passing style and takes a
continuation as its first argument. When called, the javascript function passes
the return value of @racket[proc] applied to all arguments but the first to the
function's first argument.}

@defproc[(procedure->void-js-fun [proc procedure?]) js-function?]{

Converts a @racketmodname[racket] procedure to a javascript function. The
javascript function will apply @racket[proc] to its arguments, and then ignore
@racket[proc]'s return value and return undefined (javascript's version of
returning void).}


@defproc[(prim-js->racket [v js-value?])
         (or/c boolean? inexact-real? string? vector?)]{

Converts a javascript value to its corresponding @racketmodname[racket] value.

@itemize[

 @item{When @racket[v] is a javascript boolean or string,
  @racket[prim-js->racket] returns the @racketmodname[racket] equivalent of
  that value.}
   
 @item{When @racket[v] is a number, @racket[prim-js->racket] returns an inexact
  number with the same value as the javascript number.}
  
 @item{When @racket[v] is an array, @racket[prim-js->racket] returns a vector
  whose elements are @racket[eq?] to the elements of @racket[v].}

]

If @racket[v] is not an array, boolean, function, number, or string,
@racket[prim-js->racket] will raise an @racket[exn:fail:contract] exception.}

@;-----------------------------------------------------------------------------

@section{Checking Javascript Types and Values}


The following procedures are useful for checking the values and types of
javascript values.

@defproc[(js-value? [x any/c]) boolean?]{
Returns @racket[#t] if @racket[x] is a javascript value, @racket[#f]
otherwise.}

@defproc[(js-object? [x any/c]) boolean?]{
Returns @racket[#t] if @racket[x] is a javascript function, @racket[#f]
otherwise.}

@defproc[(js-function? [x any/c]) boolean?]{
Returns @racket[#t] if @racket[x] is a javascript object, @racket[#f]
otherwise.}

@defproc[(js-=== [v1 js-value?] [v2 js-value?]) boolean?]{
Returns @racket[#t] if @racket[v1] and @racket[v2] are equal according to
javascript's @racketidfont{===}. Otherwise returns @racket[#f].}

@defproc[(js-typeof [v js-value?]) string?]{
Returns a string representing the javascript type of @racket[v]. The string is
obtained by calling javascript's @racketidfont{typeof} on @racket[v].}

@defproc[(js-instanceof [v js-value?] [type js-function?]) boolean?]{

Returns @racket[#t] if @racket[v] is an instance of the javascript type
@racket[type] and @racket[#f] otherwise.}


@defproc[(js-null? [v js-value?]) boolean?]{
Produces true if v is null.
}

@defproc[(js-undefined? [v js-value?]) boolean?]{
Produces true if v is undefined.
}




@;-----------------------------------------------------------------------------

@section{Accessing Javascript Values}


The following procedures are useful for accessing javascript values from
external APIs as well as constructing new values to pass to other javascript
functions.

@defproc[(js-get-global-value [name string?]) js-value?]{

Interprets @racket[name] as an identifier, and returns the javascript value
bound to that identifier in the global scope. If no such value exists, then a
js-value containing javascript's @racketidfont{undefined} is returned.

So, for example, @racket[(js-get-global-value "setTimeout")] would return
javascript's @racketidfont{setTimeout} procedure.}


@defproc[(js-get-field [obj js-value?] [selector string?] ...+) js-value?]{

Begins by accessing the field of @racket[obj] named by the first
@racket[selector], then accesses the field named by the second
@racket[selector] of that object, and so-on until there are no more
@racket[selector]s.

If at any point this action would result in accessing a field of a
@racketmodname[racket] value or javascript's @racketidfont{undefined},
@racket[js-get-field] will raise an @racket[exn:fail:contract] exception.}


@defproc[(js-set-field![obj (or/c js-object? js-function?)]
                       [field string?]
                       [v any/c])
         void?]{

Mutatively sets the field named @racket[field] of @racket[obj] to @racket[v]
and returns @|void-const|.}


@defproc[(js-new [constructor js-function?] [args any/c] ...) js-object?]{

Constructs a new instance of @racket[constructor] with @racket[args ...] as
arguments.}


@defproc[(js-make-hash [bindings (listof (list/c string? any/c)) empty])
         js-object?]{

Constructs a new javascript hash table. For each element of @racket[bindings],
the element is put into the hash table with the @racket[first] as the key and
the @racket[second] as the value. If multiple elements in @racket[bindings]
have @racket[first]s that are @racket[equal?], then only the last binding will
exist (the others will be overwritten).}



@defthing[js-null js-value?]{The null value.}
@defthing[js-undefined js-value?]{The undefined value.}



@defthing[minimum-js-fixnum]{The minimum number that can  be exactly representable.}
@defthing[maximum-js-fixnum]{The maximum number that can be exactly representable.}


@;-----------------------------------------------------------------------------

@section{Calling Javascript Functions}

@defproc[(js-call [f js-function?]
                  [this-arg (or/c js-object? false?)]
                  [args any/c] ...)
         any]{

Applies @racket[f] to @racket[args ...] with @racket[this-arg] as the
@racketidfont{this} object. If @racket[this-arg] is @racket[#f], then
javascript's @racketidfont{null} is used. This is equivalent to javascript's
@racketfont{f.call(this-arg, args ...)}.}

@;-----------------------------------------------------------------------------

@;@section{Examples}

@;This section contains a few simple examples of how to use the FFI.
@;
@;Example One:
@;@racketblock[
@;(js-call (js-get-global-value "setTimeout")
@;         #f
@;         (procedure->void-js-fun
@;          (lambda () (printf "tick!")))
@;         (racket->prim-js 1000))
@;]
@;This code will return @|void-const|, and after one second it will print
@;@racketoutput{tick!}. Note that this is non-blocking because javascript's
@;@racketidfont{setTimeout} is non-blocking.

@;Example Two:
@;@racketblock[
@;(define js-ht (js-make-hash '(("foo" 1) ("bar" 2))))
@;(js-set-field! js-ht "foo" 3)
@;]
@;After evaluation, @racket[js-ht] will be a javascript hash table with the
@;bindings of @racketidfont{js-ht}@racketparenfont{[}@racketvalfont{"foo"}
@;@racketparenfont{]}@racketidfont{ = }@racketvalfont{3} and
@;@racketidfont{js-ht}@racketparenfont{[}@racketvalfont{"bar"}
@;@racketparenfont{]}@racketidfont{ = }@racketvalfont{2}.
