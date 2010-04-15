#lang scheme/base
(require scheme/match
         scheme/list)

(provide (all-defined-out))

;; FIXME: the state currently isn't representing the full
;; state of the system.  I need:
;;
;; the value rib
;; the next expression to evaluate
;;
;; Also: rename retvals to accumulator
;;


;; state represents the evaluation state.
(define-struct state (stack  ;; a list of things
                      retvals)
  #:transparent)


;; state-retval: state -> any
(define (state-retval a-state)
  (unless (= 1 (length (state-retvals a-state)))
    (error 'state-retval 
           "more than one argument in retvals: ~s~n"
           (state-retvals a-state)))
  (first (state-retvals a-state)))


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


(define (state-popn a-state n)
  (cond [(= n 0)
         a-state]
        [else
         (match a-state
           [(struct state (stack retvals))
            (state-popn (make-state (rest stack) retvals)
                        (sub1 n))])]))



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
     (let loop ([depth depth]
                            [stack (state-stack a-state)])
                   (cond
                     [(= depth 0)
                      (array-ref (first stack) pos)]
                     [else
                      (loop (sub1 depth) (rest stack))]))]))


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