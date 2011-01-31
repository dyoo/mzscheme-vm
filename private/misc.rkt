#lang s-exp "profiled-base.rkt"

(require racket/contract)


;; remove-filename-extension: path-string -> path-string
;; Removes the filename extension portion.
(define (remove-filename-extension a-path)
  (let ([p (if (path? a-path)
               (path->string a-path)
               a-path)])
    (regexp-replace #px"\\.\\w+$" p "")))



(provide/contract
 [remove-filename-extension (path-string? . -> . path-string?)])