#lang s-exp "../../lang/base.ss"

(printf "begin.rkt\n")
(printf "You should see the string \"hello world\" immediately after this: ")

(begin (printf "hello ")
       (printf "world\n"))