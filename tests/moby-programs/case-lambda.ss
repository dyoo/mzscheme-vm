#lang s-exp "../../src/lang/moby-lang.ss"

(define f
  (case-lambda
   [(x) (list x)]
   [(x y) (list y x)]
   [(x y z) (list z y x)]))

(f 3)
(f 3 4)
(f 3 4 5)