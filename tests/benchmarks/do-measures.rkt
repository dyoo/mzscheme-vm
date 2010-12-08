#lang s-exp "../../lang/base.rkt"

(require "common/earley.rkt"
	 "nboyer.rkt"
	 "sboyer.rkt"
	 "tak.rkt"
	 "nfa.rkt"
	 "graphs.rkt")

"earley benchmark"
(earley-benchmark)


"(tak-benchmark)"
(tak-benchmark)



"(nboyer-benchmark 0)"
(nboyer-benchmark 0)

"(nboyer-benchmark 4)"
(nboyer-benchmark 4)


"sboyer"

