#lang s-exp "../../lang/wescheme.rkt"

(define mylst (cons 1 (cons (box #f) empty)))
(set-box! (cadr mylst) mylst)

(check-expect (format "~s\n" mylst)
	      "(1 #&...)\n")


(shared ([a (cons 1 a)])
  (check-expect (format "~s\n"a)
	        "(1 . ...)\n"))


(shared ([a (vector 1 2 a)])
  (check-expect (format "~s\n" a)
                "#(1 2 ...)\n"))

(shared ([a (box a)])
  (check-expect (format "~s\n" a)
                "#&...\n"))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define my-list (shared ([x (cons 1 (cons 2 (cons 3 y)))]
                         [y (cons 'a (cons 'b (cons 'c  (cons 'd x))))])
                  x))
(define (take n l)
  (cond
    [(= n 0)
     empty]
    [else
     (cons (first l)
           (take (sub1 n) (rest l)))]))

(check-expect (take 20 my-list)
	      '(1 2 3 a b c d 1 2 3 a b c d 1 2 3 a b c))