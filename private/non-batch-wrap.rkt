#lang s-exp "profiled-base.rkt"

(require scheme/contract
         compiler/cm)

(provide/contract 
 [unbatched-compile (path-string? . -> . path?)])


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
     (build-path (current-directory) "compiled" (path-add-suffix name #".zo"))]
    [(eq? base #f)
     ;; FIXME: I don't know what to do in this case.
     (build-path "compiled" (path-add-suffix name #".zo"))]))

