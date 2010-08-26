#lang racket/base

(require racket/contract)

(define-struct module-record (path impl))

(provide/contract 
 [struct module-record [(path (or/c module-path? false/c))
                        (impl string?)]])