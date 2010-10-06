#lang racket/base

(require racket/gui/base
         racket/unit
         racket/class
         racket/port
         framework
         drracket/tool
         
         "create-javascript-package.rkt"
         "zip-temp-dir.rkt"
         "notification-window.rkt")

;; This tool adds a "Create Javascript Package" button to the Racket menu.


(provide tool@)



(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    ;; We're not doing anything language specific, so I don't think we need
    ;; to plug into phase1 or phase2.
    (define (phase1) (void))
    (define (phase2) (void))


    
    ;; unit-frame<%>: interface
    ;; Just a helper interface used for the mixin below.
    (define unit-frame<%> (class->interface drracket:unit:frame%))

    ;; Here we mix in a menu item into the unit frame's Racket menu.
    (drracket:get/extend:extend-unit-frame
     (mixin (unit-frame<%>) (unit-frame<%>)
       (inherit get-language-menu
                get-definitions-text)
    
       
       ;; click!: menu-item% control-event% -> void
       ;; On selection, prompts for a output zip file name, and then writes a zip
       ;; with the contents.
       (define (click! a-menu-item a-control-event)
         (let* ([a-text (get-definitions-text)]
                [a-filename (send a-text get-filename)])
           (cond
             [(not (path-string? a-filename))
              (message-box "Create Javascript Package"
                           "Your program needs to be saved first before packaging.")]
             [(send a-text is-modified?)
              (message-box "Create Javascript Package"
                           "Your program has changed since your last save or load; please save before packaging.")]
             [else
              (let ([output-file
                     (finder:put-file "package.zip"
                                      #f
                                      #f
                                      "Where should the Javascript package be written to?")])
                (cond
                  [(eq? output-file #f)
                   (void)]
                  [else
                   (let ([notify-port (make-notification-window #:title "Creating Javascript Package")])
                     (let-values ([(ip dont-care)
                                   (call-with-temporary-directory->zip
                                    "package"
                                    (lambda (output-path)                                 
                                      ;; FIXME: handle error conditions!  Bad things 
                                      ;; might happen here!
                                      (fprintf notify-port "Building zip package\n")
                                      (create-javascript-package a-filename
                                                                 output-path)))])
                       (call-with-output-file output-file
                         (lambda (op) 
                           (fprintf notify-port "Writing package to file\n")
                           (copy-port ip op))
                         #:exists 'replace)))]))])))
       
       
       (super-new)
       
       (let ([racket-menu (get-language-menu)])
         (new separator-menu-item% [parent racket-menu])
         (new menu-item% 
              [parent racket-menu]
              [label "Create Javascript Package"]
              [callback click!]))))))