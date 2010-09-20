#lang racket/base

(require racket/contract)

(define-struct module-record (name path impl provides requires permissions))

;; A js-module-record is a special kind of module record whose impl
;; is Javascript rather than bytecode.
;;
;; That Javascript presumes a EXPORTS variable that's used to bind
;; its definitions.
(define-struct (js-module-record module-record) ())

(provide/contract 
 [struct module-record [(name symbol?)
                        (path path?)
                        (impl string?)
                        (provides (listof symbol?))
                        (requires (listof symbol?))
                        (permissions (listof string?))]]
 [struct (js-module-record module-record) [(name symbol?)
                                           (path path?) 
                                           (impl string?)
                                           (provides (listof symbol?))
                                           (requires (listof symbol?))
                                           (permissions (listof string?))]])


