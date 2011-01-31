#lang s-exp "profiled-base.rkt"
(require racket/match
         racket/contract)


(define (label? x)
  (symbol? x))

(define (register? x)
  (symbol? x))


(define-struct instruction ())


;; Instructions:

;; Assign a value from a where? into a register.
(define-struct (assign instruction) (name avalue))


;; An assign-value can be either a reg, a const.
(define-struct avalue ())
(define-struct (avalue:reg avalue) (name))
(define-struct (avalue:const avalue) (value))
(define-struct (avalue:label avalue) (label))


;; Apply a primitive operation and install it into a register.
(define-struct (primitive-apply/assign instruction) (name op rands))

;; Apply a primitive operation for it's side effect.
(define-struct (primitive-apply instruction) (op rands))

;; Do a primitive test.  This must be immediately followed with a branch.
(define-struct (test instruction) (op rands))

;; Conditionally jump if the preceding test is true.
(define-struct (branch instruction) (label))

;; An unconditional jump to a particular location.
;; 
(define-struct (goto instruction) (gtarget))
;; A gtarget is the target of a goto, and is either stored as a value
;; in a register, or a literal label.
(define-struct gtarget ())
(define-struct (gtarget:reg gtarget) (name))
(define-struct (gtarget:label gtarget) (label))


  




(define-struct instruction-pair (f r))




(define (append-instructions seq1 seq2)
  (make-instruction-pair seq1 seq2))



(provide/contract
 
 [struct (assign instruction) ([name register?]
                               [avalue avalue?])]
 [struct avalue ()]
 [struct (avalue:reg avalue) ([name register?])]
 [struct (avalue:const avalue) ([value any/c])]
 [struct (avalue:label avalue) ([label label?])]
 
 
 [struct (primitive-apply/assign instruction) ([name register?]
                                               [op symbol?]
                                               [rands (listof any/c)])]
 [struct (primitive-apply instruction) ([op symbol?]
                                        [rands (listof any/c)])]
 
 [struct (test instruction) ([op symbol?]
                             [rands (listof any/c)])]

 [struct (branch instruction) ([label label?])]
 
 [struct (goto instruction) ([gtarget gtarget?])]
 [struct gtarget ()]
 [struct (gtarget:reg gtarget) ([name register?])]
 [struct (gtarget:label gtarget) ([label label?])]
 
 
 [make-instruction-pair (instruction? instruction? . -> . instruction?)])
