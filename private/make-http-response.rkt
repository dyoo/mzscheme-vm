#lang racket/base

(require "version-case/version-case.rkt"
         (for-syntax racket/base))

(version-case
 [(and (version<= (version) "5.0.1")
       (version< (version) "5.1"))
  (begin
    (require web-server/http/response-structs)
    (provide make-response/basic
             TEXT/HTML-MIME-TYPE))]
 [(and (version<= (version) "5.1")
       (version< (version) "5.1.1"))
  (begin
    (require web-server/http/response-structs)
    (provide (rename-out (response make-response/basic))
             TEXT/HTML-MIME-TYPE))]
 [(and (version<= (version) "5.1.1"))
  (begin
    (require web-server/http/response-structs)
    (provide (rename-out (response make-response/basic))
             TEXT/HTML-MIME-TYPE))]
 [else
  (error 'js-vm)])

 
