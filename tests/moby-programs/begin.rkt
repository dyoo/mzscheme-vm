#lang s-exp "../../lang/wescheme.ss"

(printf "begin.rkt\n")
(printf "You should see the string \"hello world\" immediately after this: ")

(begin (printf "hello ")
       (printf "world\n"))



(let ([counter 17])
  (check-expect (begin
		  counter
		  (set! counter (add1 counter))
		  counter)
		18))


(let ([counter 17])
  (check-expect (begin0 counter
			(set! counter (add1 counter))
			counter)
		17))

"begin.rkt end"