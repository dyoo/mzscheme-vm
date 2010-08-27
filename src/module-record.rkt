#lang racket/base

(require racket/contract)

(define-struct module-record (path impl provides))

(provide/contract 
 [struct module-record [(path path?) 
                        (impl string?)
                        (provides (listof symbol?))]])
