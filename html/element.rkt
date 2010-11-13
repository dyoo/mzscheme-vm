#lang s-exp "../lang/base.rkt"
(require (for-syntax racket/base))

(define-struct element (name attrs children))

(provide (struct-out element))



;; Common tag constructors
(define-syntax (define/provide-tag stx)
  (syntax-case stx ()
    [(_ name ...)
     (with-syntax ([(<name> ...)
                    (map (lambda (name)
                           (datum->syntax name 
                                          (string->symbol 
                                           (format "<~a>" 
                                                   (syntax-e name)))))
                         (syntax->list #'(name ...)))])
       (syntax/loc stx
         (begin (define (<name> attr . elts)
                  (make-element 'name attr elts))
                ...
                (provide <name> ...))))]))



;; http://www.w3schools.com/tags/default.asp


(define/provide-tag 
  ul
  ol
  dl
  
  li
  dd
  dt
  
  a
  img
  map
  area
  p
  div
  span
  input
  textarea
  label
  fieldset
  legend
  select
  optgroup
  option
  button
  h1
  h2
  h3
  h4
  h5
  h6
  hr
  br
  b
  big
  em
  i
  small
  strong
  sub
  sup
  ins
  del
  code
  kbd
  samp
  tt
  var
  pre
  abbr
  acronym
  address
  bdo
  blockquote
  q
  cite
  dfn
  
  table
  tr
  td
  th
  caption
  colgroup
  col
  thead
  tbody
  tfoot
  
  form
  frameset
  frame
  noframes
  iframe
  )

