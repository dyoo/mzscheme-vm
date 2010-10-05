#lang racket/base

(require #;racket/gui/base
         racket/unit
         racket/class
         drracket/tool)

;; This tool adds a "Create Javascript Package" button to the Racket menu.


(provide tool@)



(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    (define (phase1)
      (void))
    
    (define (phase2)
      (void))

    (define unit-frame<%> (class->interface drracket:unit:frame%))

    ;; Here we mix in a button.
    (drracket:get/extend:extend-unit-frame
     (mixin (unit-frame<%>) (unit-frame<%>)
       (inherit get-language-menu)
       
       (super-new)
       
       (printf "~s\n" (get-language-menu))
       
       
       ))
    
    (printf "loading mzscheme-vm tool\n")))