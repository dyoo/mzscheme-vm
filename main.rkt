#lang racket/base


(require racket/contract
         racket/port
         racket/file
         racket/tcp
         racket/path
         net/sendurl
         web-server/web-server
         
         "private/misc.rkt"
         "private/create-javascript-package.rkt"
         "private/zip-temp-dir.rkt"
         "private/log-port.rkt"
         "private/suck-directory.rkt")


(provide/contract [run-in-browser (path-string? . -> . any)]
                  [create-zip-package (path-string? path-string? . -> . any)])


;; run-in-browser: path-string -> void
(define (run-in-browser a-filename)
  (log-info "Starting web server.\n")
  (let ([a-filename (normalize-path a-filename)]
        [sema (make-semaphore 0)])
    (let ([dispatcher (make-web-serving-dispatcher a-filename)])
      (let* ([port (find-open-port)]
             [url (format "http://localhost:~a/index.html" port)])
        ;; Runs the server under the user custodian
        ;; so it properly gets cleaned up.
        (serve #:dispatch dispatcher
               #:port port)
        (send-url url)
        (log-info (format 
                   "Your web application is running at ~a.  Click 'Stop' at any time to terminate the Web Server.\n"
                   url))
        
        (semaphore-wait/enable-break sema)))))

;; create-zip-package: path-string path-string -> void
;; Write out a package zip.
(define (create-zip-package a-filename output-file)
  (let ([a-filename (normalize-path a-filename)]
        [output-file (normalize-path output-file)])
    (with-handlers
        ([exn:fail? 
          (lambda (exn)
            (log-warning (format
                          "An internal error occurred during compilation: ~a\n"
                          (exn-message exn)))
            (raise exn))])
      (let-values ([(ip dont-care)
                    (call-with-temporary-directory->zip
                     (make-package-subdirectory-name output-file)
                     (lambda (output-path)                                 
                       (log-info "Compiling Javascript...\n")
                       (create-javascript-package a-filename
                                                  output-path)))])
        (call-with-output-file output-file
          (lambda (op) 
            (log-info (format "Writing package to file ~a...\n" output-file))
            (copy-port ip op))
          #:exists 'replace)
        (log-info "Done!\n")))))





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
