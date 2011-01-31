#lang s-exp "profiled-base.rkt"

(require "module-record.rkt"
         "json.rkt"
         "log-port.rkt"
         racket/list
         racket/contract)
         

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


(define (encode-interaction-record r)
  (format "{ type: 'interaction', bytecode : ~a }"
          (interaction-record-impl r)))

(define (write-interaction-record r op)
  (fprintf op 
           "{ type: 'interaction', bytecode : ~a }"
           (interaction-record-impl r)))



;; encode-module-record: module-record path -> string
(define (encode-module-record r)
  (when (not (empty? (module-record-unimplemented-primval-references r)))
    (log-warning 
     (format "WARNING: while compiling ~a, we ran across the following problem:\nThe following primitives are not yet implemented in the Javascript runtime:\n~a\nThe resulting Javascript will likely not evaluate.\n"
             (module-record-path r)
             (module-record-unimplemented-primval-references r))))
  (cond
    [(js-module-record? r)
     (format "{ type: 'js-module-record', name: ~s, provides : ~a, requires: ~a, jsImplementation : (function(STATE, EXPORTS){ (function() { ~a })() }), permissions: ~a }"
             (symbol->string (module-record-name r))
             (jsexpr->json  (map symbol->string (module-record-provides r)))
             (jsexpr->json  (map symbol->string (module-record-requires r)))
             (module-record-impl r)
             (jsexpr->json (module-record-permissions r)))]
    [else
     (format "{ type: 'module-record', name: ~s, provides : ~a, requires: ~a, bytecode : ~a, permissions: ~a }"
             (symbol->string (module-record-name r))
             (jsexpr->json  (map symbol->string (module-record-provides r)))
             (jsexpr->json  (map symbol->string (module-record-requires r)))
             (module-record-impl r)
             (jsexpr->json (module-record-permissions r)))]))

(provide/contract
 [write-module-records
  (((listof module-record?) output-port?) (#:assign-into string?) . ->* . any)]
 [encode-module-record
  (module-record? . -> . string?)]

 [encode-interaction-record
  (interaction-record? . -> . string?)]
 [write-interaction-record
  (interaction-record? output-port? . -> . any)])
