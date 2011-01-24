#lang s-exp "../profiled-base.rkt"

(require web-server/servlet
         web-server/servlet-env
         racket/runtime-path
         racket/match
         racket/port
         "../module-record.rkt"
         "../write-module-records.rkt"
         "../write-runtime.rkt"
         "../compile-moby-module.rkt"
         "module-resolver.rkt"
         "port-response.rkt"
         "json.rkt")

;; Compiler service.  Requests pass in either a module or an interaction,
;; and this service responds with the compiled module or interaction
;; toplevel.
;;
;; Parameters
;;
;;    ismodule (one-of "t" "f")
;;    version  (one-of "1")
;;    name
;;    text
;;    lang
;;
;; If lang is provided, then text is an interaction.
;; Otherwise, text is assumed to be a module. 

(define-runtime-path self-path "servlet.rkt")

(define-runtime-path wescheme-language-module 
  "../../lang/wescheme.rkt")

(define-runtime-path wescheme-interaction-language-module 
  "../../lang/wescheme-interaction.rkt")

(define-runtime-path bootstrap/bootstrap-teachpack
  "../../bootstrap/bootstrap-teachpack.rkt")


(define-runtime-path htdocs "htdocs")
(define-runtime-path support "../support")


(define-struct compilation-request
  (version module? name lang text) #:transparent)

;; Web service consuming programs and producing bytecode.
(define (start request)
  (let* ([a-compilation-request
          (parse-compilation-request request)]
         [on-error (lambda (exn)
                     (handle-exception-response 
                      a-compilation-request exn))])
    (with-handlers ([void on-error])
      (handle-regular-response a-compilation-request))))


;; parse-compilation-request: request -> compilation-request
(define (parse-compilation-request request)
  ;; lookup: string -> (or string #f)
  (define (lookup name)
    (cond [(exists-binding? name (request-bindings request))
           (extract-binding/single name (request-bindings request))]
          [else
           #f]))
  (let ([version (lookup 'version)]
        [module? (equal? (lookup 'ismodule) "t")]
        [name (lookup 'name)]
        [lang (lookup 'lang)]
        [text (lookup 'text)])
    ;; TODO: check validity of the compilation request.
    (make-compilation-request version module? name lang text)))



;; handle-regular-response: compilation-request -> response
;; Returns a response that contains JSON for the compiled program.
(define (handle-regular-response a-compilation-request)
  (cond
    [(string=? (compilation-request-version a-compilation-request)
               "1")
     (compile-1 a-compilation-request)]
    [else
     (error 'compiler "Unknown version ~e" 
            (compilation-request-version a-compilation-request))]))


(define (with-custom-module-name-resolver thunk)
  (parameterize ([current-module-name-resolver module-resolver])
    (thunk)))


(define (compile-1 a-compilation-request) 
  (match a-compilation-request
    [(struct compilation-request (version module? name lang text))
     (let-values  ([(response output-port) 
                    (make-port-response #:mime-type #"text/plain")])
       ;;;
       ;; compile-interaction: -> void
       (define (for-interaction)
         (let* ([mapped-lang
                 (cond
                   [(string=? lang "wescheme-interaction")
                    `(file ,(path->string wescheme-interaction-language-module))]
                   [else
                    (error 'compile "unknown language ~e" lang)])]
                [stx
                 (read-syntax name (open-input-string text))]
                [interaction-record 
                 (compile-interaction 
                  mapped-lang
                  stx)]
                [code (encode-interaction-record interaction-record)])
           (fprintf output-port 
                    "{\"type\":\"interaction\", \"code\":~s}"
                    code)))
       
       ;; compile-module: -> void
       (define (for-module)
         (let* ([mapped-lang
                 (cond
                   [(string=? lang "wescheme")
                    (path->string wescheme-language-module)]
                   [else
                    (error 'compile "unknown language ~e" lang)])]
                [lang-line (format "#lang s-exp (file ~s)\n" mapped-lang)]
                [bytecode-ip (get-module-bytecode/port
                              name
                              (make-module-input-port lang-line text))]
                [module-record 
                 (compile-plain-racket-module self-path
                                              self-path
                                              bytecode-ip)]
                [code (encode-module-record module-record)])
           (fprintf output-port 
                    "{\"type\":\"module\", \"code\":~s}"
                    code)))
       

       
       (define (make-module-input-port lang-line text)
         (let ([ip (open-input-string (string-append lang-line text))])
           (port-count-lines! ip)
           (let ([ip2
                  (transplant-input-port 
                   ip
                   (lambda ()
                     (let-values ([(l c p)
                                   (port-next-location ip)])
                       (cond
                         [(<= p (string-length lang-line))
                          (values l c p)]
                         [else
                          (values (sub1 l)
                                  c
                                  (- p (string-length lang-line)))])))
                   1)])
             (port-count-lines! ip2)
             ip2)))  
       
       ;;;;
       
       (with-custom-module-name-resolver
        (lambda ()
          (cond [module?
                 (for-module)]
                [else
                 (for-interaction)])))
       (close-output-port output-port)
       response)]))



;; handle-exception-response: exn -> response
(define (handle-exception-response a-compilation-request exn)
  (let ([msg (exn-message exn)])
    (make-response/full 500 
                        #"Internal Server Error"
                        (current-seconds)
                        #"application/octet-stream"
                        (list)
                        (list (string->bytes/utf-8 msg)))))

(define-struct Loc (id offset line column span))

(define (add-toplevel-dom-error-wrapper loc a-dom)
  `(span ((class "Error"))
         ,a-dom
         (span ((class "Error.location"))
               ,(Loc->dom-sexp loc))))

(define (Loc->dom-sexp a-loc)
  `(span ((class "location-reference")
          (style "display:none"))
         (span ((class "location-offset")) ,(number->string (Loc-offset a-loc)))
         (span ((class "location-line")) ,(number->string (Loc-line a-loc)))
         (span ((class "location-column")) ,(number->string (Loc-column a-loc)))
         (span ((class "location-span")) ,(number->string (Loc-span a-loc)))
         (span ((class "location-id")) ,(Loc-id a-loc))))

;; srcloc->Loc: srcloc -> Loc
;; Converts a source location (as stored in exceptions) into one that we can
;; store in error structure values.
(define (srcloc->Loc a-srcloc)
  (make-Loc (format "~a" (srcloc-source a-srcloc))
            (srcloc-position a-srcloc)
            (srcloc-line a-srcloc)
            (srcloc-column a-srcloc)
            (srcloc-span a-srcloc)))


;
;
;;; exn->structured-output: exception -> jsexpr
;;; Given an exception, tries to get back a jsexpr-structured value that can be passed back to
;;; the user.
;(define (exn->json-structured-output an-exn)
;  #;(define (on-moby-failure-val failure-val)
;      (make-hash `(("type" . "moby-failure")
;                   ("dom-message" . 
;                                  ,(dom->jsexpr 
;                                    #;(error-struct->dom-sexp failure-val #f))))))
;  (cond
;    #;[(exn:fail:read? an-exn)
;     (let ([translated-srclocs 
;            (map srcloc->Loc (exn:fail:read-srclocs an-exn))])
;       (on-moby-failure-val
;        #;(make-moby-error (if (empty? translated-srclocs)
;                             ;; Defensive: translated-srclocs should not be empty, but
;                             ;; if read doesn't give us any useful location to point to,
;                             ;; we'd better not die here.
;                             (make-Loc 0 1 0 0 "")
;                             (first translated-srclocs))
;                         (make-moby-error-type:generic-read-error
;                          (exn-message an-exn)
;                          (if (empty? translated-srclocs) 
;                              empty
;                              (rest translated-srclocs))))))]
;    
;    #;[(moby-failure? an-exn)
;     (on-moby-failure-val (moby-failure-val an-exn))]
;    
;    [else
;     (exn-message an-exn)]))
;
;
;;; dom->jsexpr: dom -> jsexpr
;;; Translate a dom structure to one that can pass through.  The dom is treated as a nested list.
;(define (dom->jsexpr a-dom)
;  (match a-dom
;    [(list head-name attribs body ...)
;     `(,(symbol->string head-name)
;       ,(map (lambda (k+v)
;               (list (symbol->string (first k+v))
;                     (second k+v))) 
;             attribs)
;       ,@(map dom->jsexpr body))]
;    [else
;     a-dom]))
;
;
;
;;; srcloc->Loc: srcloc -> jsexp
;;; Converts a source location (as stored in exceptions) into one that we can
;;; store in error structure values.
;(define (srcloc->Loc a-srcloc)
;  (void)
;  #;(make-Loc (srcloc-position a-srcloc)
;            (srcloc-line a-srcloc)
;            (srcloc-column a-srcloc)
;            (srcloc-span a-srcloc)
;            (format "~a" (srcloc-source a-srcloc))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; At initialization time, write out the support files and then start
;; up the web servlet.
(call-with-output-file (build-path htdocs "runtime.js")
  (lambda (op)
    (write-runtime "browser" op))
  #:exists 'replace)
(let ([e (build-path htdocs "evaluator.js")])
  (when (file-exists? e)
    (delete-file e))
  (copy-file (build-path support "evaluator.js") e))


(define (for-each/lazy f l)
  (cond
    [(null? l)
     (void)]
    [else
     (f (car l))
     (for-each/lazy f ((cdr l)))]))


(let ([collection-roots
       (list wescheme-language-module
             wescheme-interaction-language-module
             bootstrap/bootstrap-teachpack)])
  (call-with-output-file (build-path htdocs "modules.js")
    (lambda (op)
      (fprintf op "var MODULES = {};\n")
      (let loop ([module-records/lazy
                  (compile-moby-modules/lazy collection-roots self-path)])
        (for-each/lazy (lambda (module-record)
                         (let ([code (encode-module-record module-record)])
                           (fprintf op
                                    "MODULES[~s]=~a;\n"
                                    (symbol->string (module-record-name 
                                                     module-record))
                                    code)))
                       module-records/lazy)))
    #:exists 'replace))  



(serve/servlet start 
               #:port 8000
               #:servlet-path "/servlets/standalone.ss"
               #:extra-files-paths (list htdocs)
               #:launch-browser? #f
               #:listen-ip #f)
