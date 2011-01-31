#lang s-exp "profiled-base.rkt"
(require racket/runtime-path)

(define-runtime-path wescheme-module-path "../lang/wescheme.rkt")
  
(define wescheme-primitive-names
  (let ([ns (make-base-empty-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-require `(file ,(path->string wescheme-module-path)))
      (namespace-mapped-symbols))))

(provide wescheme-primitive-names)

