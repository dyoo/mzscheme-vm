#lang racket/base

(require web-server/servlet
         web-server/servlet-env
         racket/runtime-path
         "port-response.rkt"
         "json.rkt")

;; Compiler service.  Requests pass in either a module or an interaction,
;; and this service responds with the compiled module or interaction
;; toplevel.
;;
;; Parameters
;;
;;    version  (one-of v1)
;;    name
;;    text
;;    lang  [optional]
;;
;; If lang is provided, then text is an interaction.
;; Otherwise, text is assumed to be a module. 


(define-runtime-path htdocs "htdocs")

(define-struct compilation-request
  (version name lang text) #:transparent)

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
  (define (lookup name)
    (extract-binding/single name (request-bindings request)))
  (let ([version (lookup 'version)]
        [name (lookup 'name)]
        [lang (lookup 'lang)]
        [text (lookup 'text)])
    (make-compilation-request version name lang text)))


;; handle-regular-response: compilation-request -> response
;; Returns a response that contains JSON for the compiled program.
(define (handle-regular-response a-compilation-request)
  (let-values  ([(response output-port) 
                 (make-port-response #:mime-type #"text/plain")])
    (write-json (make-hash `(("type" . "something")
                             ("compiled-program" . "something else")))
                output-port)
    ;(display "(" output-port)
    ;#;(compile/port program-input-port output-port #:name program-name)
    ;(display ")" output-port)
    (close-output-port output-port)
    response))



;; handle-exception-response: exn -> response
(define (handle-exception-response a-compilation-request exn)
  (make-response/full 500 
                      #"Internal Server Error"
                      (current-seconds)
                      #"application/octet-stream"
                      (list)
                      (list (string->bytes/utf-8 (exn-message exn)))))


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


(serve/servlet start 
               #:port 8000
               #:servlet-path "/servlets/standalone.ss"
               #:extra-files-paths (list htdocs)
               #:launch-browser? #f
               #:listen-ip #f)
