#lang s-exp "profiled-base.rkt"
(require racket/match
         racket/contract)


(define-struct instruction ())
(define-struct where ())

;; Instructions:

(define-struct (assign instruction) (name where))


;; A where can be either a reg, a const
(define-struct (reg where) (name))
(define-struct (const where) (value))



(define-struct instruction-pair (f r))




(define (append-instructions seq1 seq2)
  (make-instruction-pair seq1 seq2))



(provide/contract
 [struct where ()]
 [struct instruction ()]

 [struct (assign instruction) ([name symbol?]
                               [where where?])]
 [struct (reg where) ([name symbol?])]
 [struct (const where) ([value any/c])]
 
 [make-instruction-pair (instruction? instruction? . -> . instruction?)])
