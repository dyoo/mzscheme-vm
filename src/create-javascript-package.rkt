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
        ;; First, write out all the collections.
        (for ([r module-records])
          (fprintf op "MODULES[~s] = ~a;\n\n"
                   (symbol->string (module-record-name r))
                   #;(path->string (find-relative-path base-dir (module-record-path r)))
                   (encode-module-record r base-dir)))
        ;; Designate one of the collections:
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


;; encode-module-record: module-record path -> string
(define (encode-module-record r base-path)
  (cond
    [(js-module-record? r)
     (format "{ name: ~s, provides : ~a, requires: ~a, jsImplementation : (function(EXPORTS){ (function() { ~a })() }), permissions: ~a }"
             (symbol->string (module-record-name r))
             (jsexpr->json  (map symbol->string (module-record-provides r)))
             (jsexpr->json  (map symbol->string (module-record-requires r)))
             (module-record-impl r)
             (jsexpr->json (module-record-permissions r)))]
    [else
     (format "{ name: ~s, provides : ~a, requries: ~a, bytecode : ~a, permissions: ~a }"
             (symbol->string (module-record-name r))
             (jsexpr->json  (map symbol->string (module-record-provides r)))
             (jsexpr->json  (map symbol->string (module-record-requires r)))
             (module-record-impl r)
             (jsexpr->json (module-record-permissions r)))]))




(provide/contract [create-javascript-package
                   (path? path? . -> . any)])