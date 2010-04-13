#lang scheme/base

(provide (all-defined-out))

;; state represents the evaluation state.
(define-struct state (retval))

;; update-state-retval: state any -> state
(define (update-state-retval a-state a-val)
  (make-state a-val))
