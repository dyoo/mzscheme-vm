#lang racket/base

(require "bytecode-structs.rkt"
         "bytecode-compiler.rkt"
         "intermediate-language.rkt"
         test-engine/racket-tests)

(check-expect (compile-top (make-compilation-top 
                            (make-prefix (list) (list))
                            (make-seq (list))))
              (make-instructions
               (list
                (make-goto 'after-indirects1)
                (make-no-op)
                'after-indirects1
                (make-install-prefix '())
                (make-no-op))))
              
(test)