#lang s-exp "moby-lang.rkt"


(provide (all-from-out "moby-lang.rkt"))



;; For compatibility, re-exports image-url as open-image-url.
(define open-image-url image-url)
(provide open-image-url)