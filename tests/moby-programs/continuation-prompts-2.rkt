#lang s-exp "../../src/lang/base.rkt"


(define (escape v)
  (abort-current-continuation
   (default-continuation-prompt-tag)
   (lambda () v)))


(printf "Running tests...\n")



(printf "testing expected value from abort with default continuation prompt tag\n")
(check-expect
 (+ 1
    (call-with-continuation-prompt
     (lambda ()
       (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (escape 6))))))))
     (default-continuation-prompt-tag)))

 7)


(check-expect
 (+ 1
    (call-with-continuation-prompt
     (lambda ()
       (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (escape 24))))))))
     (default-continuation-prompt-tag)
     (lambda (thunk)
       (printf "I see the escape\n")
       (thunk))))

 25)





(printf "tests done!\n")