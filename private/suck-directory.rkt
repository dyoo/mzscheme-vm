#lang s-exp "profiled-base.rkt"
(require racket/file
         racket/contract
         
         web-server/dispatchers/filesystem-map
         web-server/dispatchers/dispatch
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/private/mime-types
         racket/runtime-path)



;; suck-directory: path -> (hashof string bytes)
;; Given a directory, suck it all up into a hash that maps
;; filenames relative the base path to their byte contents.
(define (suck-directory a-base-path)
  (let ([ht (make-hash)])
    (let ([files (filter file-exists? (pathlist-closure 
                                       (list a-base-path)))])
      (for ([f files])
        (hash-set! ht 
                   (path->string f)
                   (file->bytes f))))
    ht))


(define-runtime-path default-mime-types "mime.types")

(define (make-default-path->mime-type)
  (make-path->mime-type default-mime-types))


(define (make-web-dispatcher a-base-path
                             #:path->mime-type 
                             (path->mime-type 
                              (make-default-path->mime-type)))
  (let ([url->path (make-url->path a-base-path)]
        [directory-cache (suck-directory a-base-path)]
        [now (current-seconds)])
    (lift:make 
     (lambda (a-req)
       (let-values ([(a-path _) (url->path (request-uri a-req))])
         (cond
           [(hash-has-key? directory-cache (path->string a-path))
            (let* ([content 
                    (hash-ref directory-cache (path->string a-path))]
                   [a-response
                    (list (path->mime-type a-path)
                          content)])
              a-response)]
           [else
            (make-response/basic
             404 
             #"Not found"
             (current-seconds)
             TEXT/HTML-MIME-TYPE (list))]))))))
  
#;(require web-server/web-server)
#;(define (test)
    (serve #:dispatch (make-web-dispatcher "/home/dyoo/work/js-vm/lang")
           #:port 8888))
  

(provide/contract [suck-directory
                   (path-string? . -> . (hash/c string? bytes?))]
                  [make-web-dispatcher
                   ((path-string?)
                    (#:path->mime-type (path? . -> . bytes?))
                    . ->* . dispatcher/c)])