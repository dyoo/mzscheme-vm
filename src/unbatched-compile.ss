#lang scheme/base

(require scheme/contract
         compiler/cm)

;; unbatched-compile: path -> path
;; Consumes a program's path, produces a path to its compiled bytecode
(define (unbatched-compile file)
  (define-values (base name dir?) (split-path file))
  (when (or (eq? base #f) dir?)
    (error 'batch "Cannot run on directory"))
  
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (managed-compile-zo file))
  (cond
    [(path? base)
     (build-path base "compiled" (path-add-suffix name #".zo"))]
    [(eq? base 'relative)
     (build-path "compiled" (path-add-suffix name #".zo"))]
    [(eq? base #f)
     (build-path "compiled" (path-add-suffix name #".zo"))]))




(provide/contract [unbatched-compile (path-string? . -> . path?)])