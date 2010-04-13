#lang scheme/base
(require scheme/match)

(provide (all-defined-out))

;; state represents the evaluation state.
(define-struct state (stack  ;; a list of things
                      retval))


;; update-state-retval: state any -> state
(define (update-state-retval a-state a-val)
  (match a-state 
    [(struct state (stack retval))
     (make-state stack a-val)]))


;; state-push: state stack-element -> state
(define (state-push a-state stack-elt)
  (match a-state
    [(struct state (stack retval))
     (make-state (cons stack-elt stack) retval)]))



(define new-state 
  (make-state '() (void)))



(define-struct array (contents length))
;; sparse, functional representation for arrays
(define (new-array n)
  (make-array (make-immutable-hash 
               (build-list n (lambda (i)
                               (cons i (void)))))
              n))

(define (array-set arr n v)
  (make-array (hash-set (array-contents arr) n v)
              (array-length arr)))

(define (array-get arr n)
  (hash-ref (array-contents arr) n))