#lang s-exp "../../lang/wescheme.rkt"


"rotate and scale"

(printf "Three images at 30, 60, 90 degree rotation:\n")

(rotate 30 (image-url "http://racket-lang.org/logo.png"))
(rotate 60 (image-url "http://racket-lang.org/logo.png"))
(rotate 90 (image-url "http://racket-lang.org/logo.png"))

"rotate and scale end"