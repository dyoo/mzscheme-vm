#lang racket

(require profile
         "../main.rkt"
         "../private/create-javascript-package.rkt"
         "../private/zip-temp-dir.rkt"
         "../private/compile-moby-module.rkt"
         racket/runtime-path)

(define-runtime-path all-tests "all-tests.rkt")

;; Profiles the building all the tests, so we can see how to
;; improve compiler performance.

(define (build-all-tests)
  (let ([p (normalize-path all-tests)])
    (call-with-temporary-directory->zip
     "foo"
     (lambda (output-path)                                 
       (create-javascript-package (normalize-path p)
                                  output-path)))))

;(profile-thunk build-all-tests)


(define (build-all-modules)
  (void (compile-moby-modules (normalize-path all-tests))))

(profile-thunk build-all-modules)