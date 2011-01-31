#lang s-exp "profiled-base.rkt"

(require racket/contract
         racket/path)

(provide/contract 
 [subdirectory-of? (path-string? path-string? . -> .
                                 boolean?)])                 

;; directory-ancestors: path -> (listof path)
(define (directory-ancestors a-path)
  (let loop ([a-path a-path])
    (let-values ([(base name dir?)
                  (split-path a-path)])
      (cond
        [(path-for-some-system? base)
         (cons base (loop base))]
        [else
         '()]))))


;; subdirectory-of?: path path -> boolean
;; Produces true if a-parent-path is an ancestor of a-path.
;; Accesses the filesystem.
(define (subdirectory-of? a-path a-parent-path)
  (let ([a-parent-path (normalize-path a-parent-path)]
        [a-path (normalize-path a-path)])
  (ormap (lambda (x)
           (= (file-or-directory-identity x)
              (file-or-directory-identity a-parent-path)))
         (cons a-path (directory-ancestors a-path)))))