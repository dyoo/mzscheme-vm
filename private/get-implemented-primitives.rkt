#lang s-exp "profiled-base.rkt"

(require racket/runtime-path
         racket/file
         racket/contract)
;; Get the list of primitives implemented in primitives.js

(define-runtime-path primitive.js "lib/primitive.js")

;; sort&unique: (listof string) -> (listof string)
(define (sort&unique names)
  (let ([ht (make-hash)])
    (for ([name names])
      (hash-set! ht name #t))
    (sort (for/list ([name (in-hash-keys ht)])
            name)
          string<?)))
        
;; primitive-names: (listof symbol)
(define primitive-names
  (map string->symbol
       (sort&unique
        (map (lambda (a-str)
               (substring a-str
                          (string-length "PRIMITIVES['")
                          (- (string-length a-str) (string-length "']"))))
             (let ([contents (file->string primitive.js)])
               (regexp-match* #px"PRIMITIVES\\[('|\")[^\\]]*('|\")\\]" contents))))))
     
(provide/contract [primitive-names (listof symbol?)])