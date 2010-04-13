#lang scheme/base
(require scheme/match
         scheme/list)

(provide (all-defined-out))

;; state represents the evaluation state.
(define-struct state (stack  ;; a list of things
                      retvals)
  #:transparent)


;; update-state-retval: state any -> state
(define (update-state-retval a-state a-val)
  (match a-state 
    [(struct state (stack retvals))
     (make-state stack (list a-val))]))

(define (update-state-retvals a-state vals)
  (match a-state 
    [(struct state (stack retvals))
     (make-state stack vals)]))


;; state-push: state stack-element -> state
(define (state-push a-state stack-elt)
  (match a-state
    [(struct state (stack retvals))
     (make-state (cons stack-elt stack) retvals)]))


;; state-peek: state -> any
(define (stack-peek a-state)
  (match a-state
    [(struct state (stack retvals))
     (first stack)]))


;; state-pop: state -> state
(define (state-pop a-state)
  (match a-state
    [(struct state (stack retvals))
     (make-state (rest stack) retvals)]))
  


;; state-install-toplevel: state number number any -> state
;; Modifies the toplevel.
(define (state-toplevel-set a-state depth pos a-val)
  (match a-state
    [(struct state (stack retvals))
     (make-state (let loop ([depth depth]
                            [stack (state-stack a-state)])
                   (cond
                     [(= depth 0)
                      (cons (array-set (first stack) pos a-val)
                            (rest stack))]
                     [else
                      (cons (first stack)
                            (loop (sub1 depth)
                                  (rest stack)))]))
                 retvals)]))


;; state-lookup-toplevel: state nonnegative-integer nonnegative-integer -> any
;; Gets at the toplevel, assuming the toplevel prefix is at the given depth.
(define (state-toplevel-ref a-state depth pos)
  (match a-state
    [(struct state (stack retvals))
     (make-state (let loop ([depth depth]
                            [stack (state-stack a-state)])
                   (cond
                     [(= depth 0)
                      (array-ref (first stack) pos)]
                     [else
                      (loop (sub1 depth) (rest stack))]))
                 retvals)]))



(define fresh-state 
  (make-state '() (list)))



(define-struct array (contents length) #:transparent)
;; sparse, functional representation for arrays
(define (new-array n)
  (make-array (make-immutable-hash 
               (build-list n (lambda (i)
                               (cons i (void)))))
              n))

(define (array-set arr n v)
  (make-array (hash-set (array-contents arr) n v)
              (array-length arr)))

(define (array-ref arr n)
  (hash-ref (array-contents arr) n))




(define-struct closure-value (name flags num-params rest? closure-values body)
  #:transparent)
