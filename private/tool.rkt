#lang s-exp "profiled-base.rkt"

(require racket/gui/base
         racket/unit
         racket/class
         racket/port
         racket/file
         racket/tcp
         net/sendurl
         framework
         drracket/tool
         
         "misc.rkt"
         "create-javascript-package.rkt"
         "zip-temp-dir.rkt"
         "notification-window.rkt"
         "log-port.rkt"
         "suck-directory.rkt"
         web-server/web-server)

;; This tool adds a "Create Javascript Package" button to the Racket menu.


(provide tool@)



;; make-reasonable-package-name: path -> string
;; Tries to pick a reasonable default for the zip file name.
(define (make-reasonable-package-name a-path)
  (let-values ([(base name dir?)
                (split-path a-path)])
    (string-append (remove-filename-extension name)
                   ".zip")))

;; make-package-subdirectory-name: path -> path
(define (make-package-subdirectory-name a-path)
  (let-values ([(base name dir?)
                (split-path a-path)])
    (remove-filename-extension name)))



;; find-open-port: -> number
;; Tries to find an open port.
(define (find-open-port)
  (let* ([T 84]
         [portno
          (let loop (;; Numerology at work  (P = 80, L = 76, T=84).
                     [portno 8076]
                     [attempts 0]) 
            (with-handlers ((exn:fail:network? (lambda (exn)
                                                 (cond [(< attempts T)
                                                        (loop (add1 portno)
                                                              (add1 attempts))]
                                                       [else
                                                        (raise exn)]))))
              ;; There's still a race condition here... Not sure how to do this right.
              (let ([port (tcp-listen portno 4 #t #f)])
                (tcp-close port)
                portno)))])
    portno))
    
    


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
                get-definitions-text
                get-interactions-text)       

       
       ;; check-cleanliness: (-> X) (-> x) (-> X) -> void
       ;; Does a check to see if the file being editing is unsaved
       ;; or dirty.
       (define (check-cleanliness #:on-unsaved on-unsaved 
                                  #:on-dirty on-dirty 
                                  #:on-ok on-ok)
         (let* ([a-text (get-definitions-text)]
                [a-filename (send a-text get-filename)])
           (cond
             [(not (path-string? a-filename))
              (on-unsaved)]
             [(send a-text is-modified?)
              (on-dirty)]
             [else
              (on-ok)])))
         
       
       ;; click!: menu-item% control-event% -> void
       ;; On selection, prompts for a output zip file name,
       ;; and then writes a zip with the contents.
       (define (click! a-menu-item a-control-event)
         (let* ([a-text (get-definitions-text)]
                [a-filename (send a-text get-filename)])
           (check-cleanliness 
            #:on-unsaved
            (lambda ()
              (message-box "Create Javascript Package"
                           "Your program needs to be saved first before packaging."))
            
            #:on-dirty
            (lambda ()
              (message-box "Create Javascript Package"
                           "Your program has changed since your last save or load; please save before packaging."))
            
            #:on-ok
            (lambda ()
              (let ([output-file
                     (finder:put-file (make-reasonable-package-name a-filename)
                                      #f
                                      #f
                                      "Where should the Javascript package be written to?"
                                      )])
                (cond
                  [(eq? output-file #f)
                   (void)]
                  [else
                   (let ([notify-port 
                          (make-notification-window 
                           #:title "Creating Javascript Package")])
                     (parameterize ([current-log-port notify-port])
                       (with-handlers
                           ([exn:fail? 
                             (lambda (exn)
                               (fprintf notify-port
                                        "An internal error occurred during compilation: ~a\n"
                                        (exn-message exn))
                               (raise exn))])
                         (let-values ([(ip dont-care)
                                       (call-with-temporary-directory->zip
                                        (make-package-subdirectory-name output-file)
                                        (lambda (output-path)                                 
                                          (fprintf notify-port "Compiling Javascript...\n")
                                          (create-javascript-package a-filename
                                                                     output-path)))])
                           (call-with-output-file output-file
                             (lambda (op) 
                               (fprintf notify-port "Writing package to file ~a...\n" output-file)
                               (copy-port ip op))
                             #:exists 'replace)
                           (fprintf notify-port "Done!\n")))))]))))))
       
       
       
       ;; make-web-serving-dispatcher: path -> dispatcher/c
       (define (make-web-serving-dispatcher a-filename)
         (let* ([tmpdir
                 (make-temporary-file "mztmp~a"
                                      'directory
                                      #f)])
           (dynamic-wind 
            (lambda () (void))
            (lambda ()
              (create-javascript-package a-filename tmpdir)
              (make-web-dispatcher tmpdir))
            (lambda () (delete-directory/files tmpdir)))))
       
       
       (define (run! a-menu-item a-click-event)
         (check-cleanliness 
            #:on-unsaved
            (lambda ()
              (message-box "Run Javascript in Browser"
                           "Your program needs to be saved first before we can Javascript-compile and run it."))
            
            #:on-dirty
            (lambda ()
              (message-box "Run Javascript in Browser"
                           "Your program has changed since your last save or load; please save first."))
            
            #:on-ok
            (lambda ()
              (parameterize ([current-custodian (let* ([a-rep (get-interactions-text)])
                                                  (send a-rep get-user-custodian))])
                (let ([notify-port 
                       (make-notification-window 
                        #:title "Running Javascript")])
                  (parameterize ([current-log-port notify-port])
                    (fprintf notify-port "Starting up web server.\n")
                    (let* ([a-text (get-definitions-text)]
                           [a-filename 
                            (send a-text get-filename)]
                           [dispatcher 
                            (make-web-serving-dispatcher a-filename)])
                      
                      (let* ([port (find-open-port)]
                             [url (format "http://localhost:~a/index.html" port)])
                        ;; Runs the server under the user custodian
                        ;; so it properly gets cleaned up.
                        (serve #:dispatch dispatcher
                               #:port port)
                        (send-url url)
                        (fprintf notify-port 
                                 "Server should be running on ~a, and will stay up until the next Run.\n"
                                 url)))))))))
       
              
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;; Initialization
       (super-new)       
       (let ([racket-menu (get-language-menu)])
         (new separator-menu-item% [parent racket-menu])
         (new menu-item% 
              [parent racket-menu]
              [label "Create Javascript Package"]
              [callback click!])
         
         (new menu-item%
              [parent racket-menu]
              [label "Run Javascript in Browser"]
              [callback run!])
         )))))