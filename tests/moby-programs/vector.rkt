#lang s-exp "../../lang/wescheme.rkt"

(printf "vector.rkt\n")


(define v (build-vector 5 (lambda (a) a)))
(check-expect v #(0 1 2 3 4))
(check-expect (vector-length v) 5)

(check-expect (vector? v) true)
(check-expect (vector? '(not a vector)) false)


(define v2 (build-vector 5 (lambda (a) (* a a))))
(check-expect v2 #(0 1 4 9 16))

(check-expect (vector->list #()) '())
(check-expect (vector->list v2) '(0 1 4 9 16))


(check-expect (list->vector '()) #())
(check-expect (list->vector '(a b c)) #(a b c))


(define v3 (vector 'hello 'world))
(check-expect v3 '#(hello world))
(vector-set! v3 0 'hola)
(check-expect v3 '#(hola world))
(check-expect (vector-ref v3 0) 'hola)




(printf "vector.rkt end\n")

