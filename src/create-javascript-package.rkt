#lang racket/base


(require "write-runtime.rkt"
         "compile-moby-module.rkt"
         "module-record.rkt"
         "private/json.rkt"
         "log-port.rkt"
         racket/list
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


;; write-module-records: (listof module-record) path output-port -> void
;; Writes out the module records.  Each module record will be written out as
;; an assignment into the MODULES object by default.
;;    MODULES[module-name] = ...
(define (write-module-records module-records op #:assign-into (assign-into "MODULES"))
  (for ([r module-records])
    (fprintf op "~a[~s] = ~a;\n\n"
             assign-into
             (symbol->string (module-record-name r))
             #;(path->string (find-relative-path base-dir (module-record-path r)))
             (encode-module-record r))))


;; encode-module-record: module-record path -> string
(define (encode-module-record r)
  (when (not (empty? (module-record-unimplemented-primval-references r)))
    (log-warning 
     (format "WARNING: while compiling ~a, we ran across the following problem:\nThe following primitives are not yet implemented in the Javascript runtime:\n~a\nThe resulting Javascript will likely not evaluate.\n"
             (module-record-path r)
             (module-record-unimplemented-primval-references r))))
  (cond
    [(js-module-record? r)
     (format "{ name: ~s, provides : ~a, requires: ~a, jsImplementation : (function(STATE, EXPORTS){ (function() { ~a })() }), permissions: ~a }"
             (symbol->string (module-record-name r))
             (jsexpr->json  (map symbol->string (module-record-provides r)))
             (jsexpr->json  (map symbol->string (module-record-requires r)))
             (module-record-impl r)
             (jsexpr->json (module-record-permissions r)))]
    [else
     (format "{ name: ~s, provides : ~a, requires: ~a, bytecode : ~a, permissions: ~a }"
             (symbol->string (module-record-name r))
             (jsexpr->json  (map symbol->string (module-record-provides r)))
             (jsexpr->json  (map symbol->string (module-record-requires r)))
             (module-record-impl r)
             (jsexpr->json (module-record-permissions r)))]))




(provide/contract [create-javascript-package
                   (path? path? . -> . any)]
                  [write-module-records
                   (((listof module-record?) output-port?) (#:assign-into string?) . ->* . any)])