#lang typed/racket/base

(provide (all-defined-out))

;; An instruction is one of the following:
(define-type instruction (U assignment
                            goto
                            label
                            no-op
                            install-prefix
                            instructions))




;; The linkage between instructions can be one of these:
(define-type linkage (U 'return
                        'next
                        label))
                        


;; Instructions:

;; Assign a value from a where? into a register.
(define-struct: assignment ([name : Symbol]
                            [value : (U reg const label)])
  #:transparent)

;; An assign-value can be either a reg, a const.
(define-struct: reg ([name : Symbol]) #:transparent)
(define-struct: const ([value : Any]) #:transparent)
(define-type label Symbol)


;; An instruction sequence sequentially appends instructions together.
(define-struct: instructions ([seqs : (Listof instruction)])
  #:transparent)


(: instruction-append (instruction * -> instruction))
;; Append instructions together into one thing.
(define (instruction-append . seqs)
  (cond [(null? seqs)
         (make-no-op)]
        [else
         (make-instructions seqs)]))



;; An unconditional jump to a particular location.
;; 
(define-struct: goto ([target : (U reg label)])
  #:transparent)


;; no-op: do nothing instruction.
(define-struct: no-op ()
  #:transparent)


(define-struct: install-prefix ([tops : (Listof Toplevel)])
  #:transparent)

(define-type Toplevel (U #f
                         Symbol
                         GlobalBucket
                         ModuleVariable))
(define-struct: GlobalBucket ([name : Symbol])
  #:transparent)
(define-struct: ModuleVariable ([name : Symbol]
                                [modidx : Any]
                                [pos : Any]
                                [phase : Any])
  #:transparent)


(define-predicate instruction? instruction)
