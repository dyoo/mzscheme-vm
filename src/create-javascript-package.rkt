#lang racket/base


(require "write-runtime.rkt"
         "compile-moby-module.rkt"
         "module-record.rkt"
         "private/json.rkt"
         racket/path
         racket/contract)

  
;; moby: path -> void
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
    
    ;; Write out the translated program.
    ;; FIXME: write out all the other used modules too.
    (call-with-output-file (build-path output-directory "program.js")
      (lambda (op)
        (for ([r module-records])
          (cond
            [(string=? (path->string (module-record-path r))
                       (path->string a-path))
             (fprintf op "var program = (~a);\n\n" 
                      (module-record-impl r))]
            [else
             (fprintf op "COLLECTIONS[~s] = ~a;\n\n"
                      (path->string (find-relative-path base-dir (module-record-path r)))
                      (encode-module-record r base-dir))])))
      #:exists 'replace)))


;; encode-module-record: module-record path -> string
(define (encode-module-record r base-path)
  (format "{ name: ~s, provides : ~a, bytecode : ~a }"
          (path->string 
           (find-relative-path base-path
                               (module-record-path r)))
          (jsexpr->json  (map symbol->string (module-record-provides r)))
          (module-record-impl r)))




(provide/contract [create-javascript-package
                   (path? path? . -> . any)])