#lang s-exp "../../lang/wescheme.rkt"


"cycle tests"

(define mylst (cons 1 (cons (box #f) empty)))
(set-box! (cadr mylst) mylst)


;; FIXME: the output format for cycles in the Javascript implementation is
;; printing out ... rather than the traditional format.
;; 
;; This is a bad deviation.

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







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-expect (shared ([x (list 1 2)]) x)
	      (list 1 2))

(check-expect (shared ([x (vector 1 2)]) x)
	       #(1 2))

(check-expect (shared ([x (box 1)]) x)
              (box 1))

(check-expect (shared ([x (cons 1 null)]) x)
	      '(1))



;;(stest (x "#1=(#1# 1)") '(shared ([x (list x 1)]) x))
(let ([result (shared ([x (list x 1)]) x)])
  (check-expect result (car result))
  (check-expect (cdr result) '(1)))

;(stest (x "#2=#(#2# 1)") '(shared ([x (vector x 1)]) x))
(let ([result (shared ([x (vector x 1)]) x)])
  (check-expect (vector-ref result 0) result)
  (check-expect (vector-ref result 1) 1)
  (check-expect (vector-length result) 2))

;(stest (x "#2=#(#2# 1)") '(shared ([x (vector-immutable x 1)]) x))
;(let ([result (shared ([x (vector-immutable x 1)]) x)])
;  (check-expect (vector-ref result 0) result)
;  (check-expect (vector-ref result 1) 1)
;  (check-expect (vector-length result) 2))


;(stest (x "#3=#&#3#") '(shared ([x (box x)]) x))
(let ([result (shared ([x (box x)]) x)])
  (check-expect (unbox result) result))

;(stest (x "#3=#&#3#") '(shared ([x (box-immutable x)]) x))


;(stest (x "#4=(#4#)") '(shared ([x (cons x null)]) x))
(let ([result (shared ([x (cons x null)]) x)])
  (check-expect (car result) result)
  (check-expect (cdr result) null))



;(stest (x "#5=(1 . #5#)") '(shared ([x (cons 1 x)]) x))
(let ([result (shared ([x (cons 1 x)]) x)])
  (check-expect (car result) 1)
  (check-expect (cdr result) result))


;; (stest (x "#11=(#11#)") '(shared ([x `(,x)]) x))
(let ([result (shared ([x `(,x)]) x)])
  (check-expect (length result) 1)
  (check-expect (car result) result))







"cycle tests done"

