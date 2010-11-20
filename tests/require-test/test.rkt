#lang racket/base

;; check to see that multiple invokations don't duplicate code generation.

(require "../../private/compile-moby-module.rkt"
         "../../private/module-record.rkt"
         racket/runtime-path)

(define-runtime-path m.rkt 
  "/home/dyoo/Downloads/tmp/Package/tourguide.rkt")

(define (check-module-names-unique! module-records)
  (let ([names (map module-record-name module-records)])
    names))



(define (test)
  (define modules 
    (compile-moby-modules m.rkt))
  (check-module-names-unique! modules))


(test)