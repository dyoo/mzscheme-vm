#lang scheme/base

(define (fact x)
  (cond
    ((= x 0) 1)
    (else (* x (fact (sub1 x))))))

(fact 100)
