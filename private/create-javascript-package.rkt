#lang s-exp "profiled-base.rkt"


(require "write-runtime.rkt"
         "write-module-records.rkt"
         "module-record.rkt"
         "compile-moby-module.rkt"
         racket/path
         racket/contract)

  
;; create-javascript-package: path -> void
;; Given the path of a scheme program, run it through the compiler
;; and generate the javascript program.
(define (create-javascript-package a-path output-directory)
  (let*-values ([(a-path) (normalize-path a-path)]
                [(base-dir file dir?) (split-path a-path)]
                [(module-records) (compile-moby-modules a-path)])

    ;; Write out the support runtime.
    (call-with-output-file (build-path output-directory "runtime.js")
      (lambda (op)
        (write-runtime "browser" op))
      #:exists 'replace)

    (copy-support-files output-directory)
    
    (copy-js-compatibility-libraries output-directory)
    
    ;; Write out the translated program.
    ;; FIXME: write out all the other used modules too.
    (call-with-output-file (build-path output-directory "program.js")
      (lambda (op)
        ;; First, write out all the collections.
        (write-module-records module-records op)
        
        ;; Designate one of the collections as the main one.
        (for ([r module-records])
          (cond
            [(string=? (path->string (module-record-path r))
                       (path->string a-path))
             (fprintf op "var programModuleName = ~s;\n\n" 
                      (symbol->string (module-record-name r))
                      #;(path->string (find-relative-path base-dir (module-record-path r))))]
            [else
             (void)])))
      #:exists 'replace)))



(provide/contract [create-javascript-package
                   (path? path? . -> . any)])