#lang racket/base

(require "version-case/version-case.rkt"
         (for-syntax racket/base))

(version-case
 
 [(and (version<= "5.0.1" (version))
       (version< (version) "5.1"))
  (begin
    (require web-server/http/response-structs)
    
    (define (my-make-response/basic code message content time mime headers)
      (make-response/full 
       code
       message
       time
       mime 
       headers
       (list content)))
    
    (define (make-response/sexp an-sexp)
      an-sexp)
    
    (provide (rename-out (my-make-response/basic make-response/basic))
             make-response/sexp
             TEXT/HTML-MIME-TYPE))]
 
 [(version<= "5.1" (version))
  (begin
    (require web-server/http/response-structs
             web-server/http/xexpr)
    (define (my-make-response/basic code message content time mime headers)
      (response code message time mime headers
                (lambda (op)
                  (write-bytes content op))))
    
    (define (make-response/sexp an-sexp)
      (response/xexpr an-sexp))
    
    (provide (rename-out (my-make-response/basic make-response/basic))
             make-response/sexp
             TEXT/HTML-MIME-TYPE))]
 [else
  (error 'js-vm "Unsupported Racket version ~s" (version))])


