#lang s-exp "profiled-base.rkt"
(require racket/runtime-path)

(define kernel-primitive-names
  (let ([ns (make-base-empty-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-require `'#%kernel)
      (namespace-mapped-symbols))))


(define paramz-primitive-names
  (let ([ns (make-base-empty-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-require `'#%paramz)
      (namespace-mapped-symbols))))
	

(provide kernel-primitive-names
	 paramz-primitive-names)

