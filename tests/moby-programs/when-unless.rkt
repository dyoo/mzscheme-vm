#lang s-exp "../../src/lang/moby-lang.ss"


(when (= (expt 2 100)
	 1267650600228229401496703205376)
  'ok)

(unless (not (= (expt 2 100)
	   1/1267650600228229401496703205376))
  'not-ok)