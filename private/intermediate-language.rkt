#lang typed/racket/base

(provide (all-defined-out))

;; An instruction is one of the following:
(define-type instruction (U assignment
                            primitive-application/assignment
                            primitive-application
                            test
                            branch
                            goto
                            instruction-sequence))

;; Instructions:

;; Assign a value from a where? into a register.
(define-struct: assignment ([name : Symbol]
                            [value : (U reg const label)]))

;; An assign-value can be either a reg, a const.
(define-struct: reg ([name : Symbol]))
(define-struct: const ([value : Any]))
(define-struct: label ([label : Symbol]))


;; An instruction sequence sequentially appends instructions together.
(define-struct: instruction-sequence ([seq1 : instruction]
                                      [seq2 : instruction]))


(: instruction-sequence-append (instruction instruction -> instruction))
;; Append two instructions together.
(define (instruction-sequence-append seq1 seq2)
  (make-instruction-sequence seq1 seq2))


(define-type Primitive (U #;... ))


;; Apply a primitive operation and install it into a register.
(define-struct: primitive-application/assignment ([name : Symbol]
                                                  [op : Primitive]
                                                  [rands : (Listof Any)]))

;; Apply a primitive operation for it's side effect.
(define-struct: primitive-application ([op : Primitive]
                                       [rands : (Listof Any)]))

;; Do a primitive test.  This must be immediately followed with a branch.
(define-struct: test ([op : Primitive] 
                      [rands : (Listof Any)]))

;; Conditionally jump if the preceding test is true.
(define-struct: branch ([label : Symbol]))

;; An unconditional jump to a particular location.
;; 
(define-struct: goto ([target : (U reg label)]))
