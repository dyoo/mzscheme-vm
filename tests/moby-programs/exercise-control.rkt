#lang s-exp "../../src/lang/moby-lang.ss"
(printf "~s~n" (if true
		   'ok
		   'not-ok))

(printf "~s~n" (if false
		   'not-ok
		   'ok))

(printf "~s~n" (cond [true 'ok]
		     [else 'not-ok]))

(printf "~s~n" (cond [false 'not-ok]
		     [else 'ok]))

(printf "~s~n" (case 42
		 [(1) 'not-ok]
		 [(2) 'not-ok]
		 [(42) 'ok]))

(printf "~s~n" (case 42
		 [(1) 'not-ok]
		 [(2) 'not-ok]
		 [(42) 'ok]))