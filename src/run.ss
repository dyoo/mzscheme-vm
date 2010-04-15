#lang scheme/base

(require scheme/match
         scheme/list
         compiler/zo-parse
         "structs.ss")


;; The structure of the code follows the type definitions in:
;; http://docs.plt-scheme.org/mzc/decompile.html?q=zo-parse#(def._((lib._compiler/zo-parse..ss)._indirect~3f))



;; This is a simple evaluator of the VM.  Afterwards, we'll be working on
;; the actual compiler to Javascript.



;; run-primitives: toplevel state -> state
(define (run a-top state)
  (run-top a-top state))


;; run-top: top state -> state
(define (run-top a-top state)
  (match a-top
    [(struct compilation-top (max-let-depth prefix code))
     (begin
       ;; fixme: handle max-let-depth and prefix.
       (run-code code state))]))


;; run-code: code state -> state
(define (run-code a-code state)
  (match a-code
    [(? form?)
     (run-form a-code state)]
    [(? indirect?)
     (run-indirect a-code state)]
    [else
     ;; literal value is self-evaluating
     (update-state-retval state a-code)]))



;; run-form: form state -> state
(define (run-form a-form state)
  (match a-form
    [(? def-values?)
     (run-def-values a-form state)]
    [(? def-syntaxes?)
     (run-def-syntaxes a-form state)]
    [(? def-for-syntax?)
     (run-def-for-syntax a-form state)]
    [(? req?)
     (run-req a-form state)]
    [(? seq?)
     (run-seq a-form state)]
    [(? splice?)
     (run-splice a-form state)]
    [(? mod?)
     (run-mod a-form state)]
    [(? expr?)
     (run-expr a-form state)]))


;; run-mod: mod state -> state
(define (run-mod a-mod state)
  (match a-mod
    [(struct mod (name
                  self-modidx
                  prefix
                  provides
                  requires
                  body
                  syntax-body
                  unexported
                  max-let-depth
                  dummy
                  lang-info
                  internal-context))
     (let ([new-state
            (extend-state-with-prefix state prefix)])
       ;; FIXME: handle the variables here that we're not
       ;; looking at, like prefix and max-let-depth.
       (foldl (lambda (b state)
                (match b 
                  [(? form?)
                   (run-form b state)]
                  [(? indirect?)
                   (run-indirect b state)]
                  [else
                   (update-state-retval state b)]))
              new-state
              body))]))



;; extend-state-with-prefix: state prefix -> state
(define (extend-state-with-prefix state a-prefix)
  (match a-prefix
    [(struct prefix (numlifts toplevels stxs))
     
     (state-push state (new-array (+ (length toplevels)
                                     (if (empty? stxs)
                                         0
                                         1)
                                     numlifts)))]))


;; run-splice: splice state -> state
(define (run-splice a-splice state)
  (match a-splice
    [(struct splice (forms))
     (foldl (lambda (f state)
              (match f
                [(? form?)
                 (run-form f state)]
                [(? indirect?)
                 (run-indirect f state)]
                [else
                 (update-state-retval state f)]))
            state
            forms)]))


;; run-req: req state -> state
(define (run-req a-req state)
  (match a-req 
    [(struct req (reqs dummy))
     state
     ;; FIXME
     #;(list)]))


;; evaluate-at-expression-position: (U expr seq indirect any) state -> state
;;
;; evaluate the expression-like thing at x, installing it into the retvals of
;; the current state.
(define (evaluate-at-expression-position x state)
  (match x
    [(? expr?)
     (run-expr x state)]
    [(? seq?)
     (run-seq x state)]
    [(? indirect?)
     (run-indirect x state)]
    [else
     (update-state-retval state x)]))


;; run-def-values: def-values state -> state
;; Accumulates the values for rhs, and then installs each value in turn
;; into the toplevel.
(define (run-def-values a-def-values state)
  (match a-def-values
    [(struct def-values (ids rhs))
     (let ([state-with-retvals
            (evaluate-at-expression-position rhs state)])
       (foldl (lambda (a-toplevel a-val a-state)
                (match a-toplevel
                  [(struct toplevel (depth pos const? ready?))
                   (state-toplevel-set a-state depth pos a-val)]))
              state-with-retvals
              ids
              (state-retvals state-with-retvals)))]))


(define (run-def-syntaxes a-def-syntaxes state)
  (match a-def-syntaxes
    [(struct def-syntaxes (ids rhs prefix max-let-depth))
     state
     ;; FIXME
     #;(append (match rhs
                 [(? expr?)
                  (run-expr rhs)]
                 [(? seq?)
                  (run-seq rhs)]
                 [(? indirect?)
                  (run-indirect rhs)])
               (run-prefix prefix))]))

(define (run-def-for-syntax a-def-for-syntax state)
  (match a-def-for-syntax
    [(struct def-for-syntax (ids rhs prefix max-let-depth))
     state
     ;; FIXME
     #;(append (match rhs
                 [(? expr?)
                  (run-expr rhs)]
                 [(? seq?)
                  (run-seq rhs)]
                 [(? indirect?)
                  (run-indirect rhs)]
                 [else
                  (list)])
               (run-prefix prefix)
               (run-max-let-depth max-let-depth))]))






#;(define (run-provided a-provided)
    (match a-provided
      [(struct provided (name src src-name nom-mod src-phase protected? insp))
       (list)]))





;                                          
;                                          
;                                          
;                                          
;                                          
;                                          
;     ;;;;    ;;    ;   ;;;;;;      ; ;;;; 
;    ;;  ;;    ;;  ;;   ;;;  ;;     ;;;    
;   ;;    ;     ; ;;    ;;    ;     ;;     
;   ;;    ;;    ;;;     ;;    ;;    ;      
;   ;;;;;;;;     ;;     ;;    ;;    ;      
;   ;;          ;;;;    ;;    ;;    ;      
;   ;;         ;;  ;    ;;    ;     ;      
;    ;;   ;    ;   ;;   ;;;  ;;     ;      
;     ;;;;;   ;;    ;;  ;; ;;;      ;      
;                       ;;                 
;                       ;;                 
;                       ;;                 
;                                  ;       



(define (run-expr an-expr state)
  (match an-expr
    [(? lam?)
     (run-lam an-expr state)]
    [(? closure?)
     (run-closure an-expr state)]
    [(? indirect?)
     (run-indirect an-expr state)]
    [(? case-lam?)
     (run-case-lam an-expr state)]
    [(? let-one?)
     (run-let-one an-expr state)]
    [(? let-void?)
     (run-let-void an-expr state)]
    [(? install-value?)
     (run-install-value an-expr state)]
    [(? let-rec?)
     (run-let-rec an-expr state)]
    [(? boxenv?)
     (run-boxenv an-expr state)]
    [(struct localref (unbox? pos clear? other-clears? flonum?))
     (run-localref an-expr state)]
    [(? toplevel?)
     (run-toplevel an-expr state)]
    [(? topsyntax?)
     (run-topsyntax an-expr state)]
    [(? application?)
     (run-application an-expr state)]
    [(? branch?)
     (run-branch an-expr state)]
    [(? with-cont-mark?)
     (run-with-cont-mark an-expr state)]
    [(? beg0?)
     (run-beg0 an-expr state)]
    [(? varref?)
     (run-varref an-expr state)]
    [(? assign?)
     (run-assign an-expr state)]
    [(? apply-values?)
     (run-apply-values an-expr state)]
    [(? primval?)
     (run-primval an-expr state)]))


;; run-lam: lam state -> state
(define (run-lam a-lam state)
  (match a-lam
    [(struct lam (name flags num-params param-types 
                       rest? closure-map closure-types 
                       max-let-depth body))
     (update-state-retval state
                          (make-closure-value name
                                              flags
                                              num-params
                                              rest?
                                              (capture-closure-map-values
                                               closure-map state)
                                              body))]))


;; capture-closure-map-values: (vectorof exact-nonnegative-integer) state -> (listof any)
;; Captures the values of the free-variables for the closure.
(define (capture-closure-map-values closure-map state)
  (build-list (vector-length closure-map)
              (lambda (i)
                (list-ref (state-stack state) (vector-ref closure-map i)))))



;; run-closure: closure state -> state
(define (run-closure a-closure state)
  (match a-closure 
    [(struct closure (lam gen-id))
     (let ([new-state (run-lam lam state)])
       ;; FIXME: squirrel away the gen-id into the evaluated closure.
       new-state)]))


;; run-indirect: indirect state -> state
(define (run-indirect an-indirect state)
  (match an-indirect
    [(struct indirect (v))
     (let ([new-state (run-closure v state)])
       new-state)]))


;; run-case-lam: case-lam state -> state
(define (run-case-lam a-case-lam state)
  (match a-case-lam
    [(struct case-lam (name clauses))
     state
     ;; FIXME
     #;(apply append (map run-lam clauses))]))


;; run-let-one: let-one state -> state
(define (run-let-one a-let-one state)
  (match a-let-one
    [(struct let-one (rhs body flonum?))
     state
     ;; FIXME
     #;(append (match rhs
                 [(? expr?)
                  (run-expr rhs)]
                 [(? seq?)
                  (run-seq rhs)]
                 [(? indirect?)
                  (run-indirect rhs)]
                 [else
                  (list)])
               (match body
                 [(? expr?)
                  (run-expr body)]
                 [(? seq?)
                  (run-seq body)]
                 [(? indirect?)
                  (run-indirect body)]
                 [else
                  (list)]))]))

;; run-let-void: let-void state -> satte
(define (run-let-void a-let-void state)
  (match a-let-void
    [(struct let-void (count boxes? body))
     state
     ;; FIXME
     #;(match body
         [(? expr?)
          (run-expr body)]
         [(? seq?)
          (run-seq body)]
         [(? indirect?)
          (run-indirect body)]
         [else
          (list)])]))



;; run-install-value: install-value state -> state
(define (run-install-value an-install-value state)
  (match an-install-value
    [(struct install-value (count pos boxes? rhs body))
     state
     #;(append (match rhs
                 [(? expr?)
                  (run-expr rhs)]
                 [(? seq?)
                  (run-seq rhs)]
                 [(? indirect?)
                  (run-indirect rhs)]
                 [else
                  (list)])
               (match body
                 [(? expr?)
                  (run-expr body)]
                 [(? seq?)
                  (run-seq body)]
                 [(? indirect?)
                  (run-indirect body)]
                 [else
                  (list)]))]))

;; run-let-rec: let-rec state -> state
(define (run-let-rec a-let-rec state)
  (match a-let-rec
    [(struct let-rec (procs body))
     state
     ;; FIXME
     #;(append (apply append (map run-lam procs))
               (match body
                 [(? expr?)
                  (run-expr body)]
                 [(? seq?)
                  (run-seq body)]
                 [(? indirect?)
                  (run-indirect body)]
                 [else
                  (list)]))]))


;; run-boxenv: boxenv state -> state
(define (run-boxenv a-boxenv state)
  (match a-boxenv
    [(struct boxenv (pos body))
     ;; FIXME
     state
     #;(match body
         [(? expr?)
          (run-expr body)]
         [(? seq?)
          (run-seq body)]
         [(? indirect?)
          (run-indirect body)]
         [else
          (list)])]))



;; run-localref: localref state -> state
(define (run-localref a-localref state)
  (match a-localref
    [(struct localref (unbox? pos clear? other-clears? flonum?))
     #;(printf "local reference gets back ~s~n" (state-local-ref state pos)) 
     (update-state-retval state (state-local-ref state pos))
     #;(list)]))



;; run-toplevel: toplevel state -> state
;; Put the toplevel reference onto the retval register.
(define (run-toplevel a-toplevel state)
  (match a-toplevel
    [(struct toplevel (depth pos const? ready?))
     (update-state-retval state (state-toplevel-ref state depth pos))]))


;; run-topsyntax: topsyntax state -> state
(define (run-topsyntax a-topsyntax state)
  (match a-topsyntax
    [(struct topsyntax (depth pos midpt))
     state
     #;(list)]))


;; run-application: application state -> state
(define (run-application an-application state)
  (match an-application
    [(struct application (rator rands))
     (let* ([old-rib (state-value-rib state)]
            [state-with-rator (evaluate-at-expression-position rator state)]
            [rator-val (state-retval state-with-rator)]
            [state-with-rands 
             (foldl (lambda (rand state)
                      (let ([new-rand-val+state
                             (evaluate-at-expression-position 
                              rand 
                              state)])
                        (state-add-value-to-rib new-rand-val+state
                         (state-retval 
                          new-rand-val+state))))
                    state-with-rator
                    (reverse rands))]
            [rands-val (state-value-rib state-with-rands)])
       (apply-operator rator-val rands-val 
                       (state-replace-value-rib state-with-rands
                                                old-rib)))]))


;; evaluate-many-at-expression-position: (listof expression-position) state -> state
(define (evaluate-many-at-expression-position rands state)
  (cond
    [(empty? rands)
     state]
    [else
     (let* ([evaluated-rands/rev (state-retvals state)]
            [new-state (evaluate-at-expression-position (first rands) state)]
            [new-evaluated-rands/rev (cons (state-retval new-state) 
                                           evaluated-rands/rev)])
       (evaluate-many-at-expression-position 
        (rest rands) 
        (update-state-retvals new-state
                              new-evaluated-rands/rev)))]))



;; apply-operator: value (listof value) state -> state
(define (apply-operator rator rands state)
  #;(printf "In apply-operator, with rator=~s~n~n and rands~s~n~n" rator rands)
  #;(newline)
  (match rator
    [(struct closure-value (name flags num-params rest? closure-values body))
     (cond [rest?
            (let* (;; push the rest arguments
                   [state (state-push state (list-tail rands num-params))]

                   ;; push the normal arguments
                   [state (foldl (lambda (v state)
                                   (state-push state v))
                                 state
                                 (reverse rands))]
                   ;; push the closure values
                   [state (foldl (lambda (v state)
                                   (state-push state v))
                                 state 
                                 (reverse closure-values))])
              
              
              ;; evaluate the body
              (let ([new-state 
                     (begin
                       #;(printf "Evaluating body ~s~n" body)
                       (evaluate-at-expression-position body state))])
                ;; Pop off values from the stack.
                (state-popn new-state (+ 1 
                                         (length rands)
                                         (length closure-values)))))]
           [else
            (let* (;; push the normal arguments
                   [state (foldl (lambda (v state)
                                   (state-push state v))
                                 state
                                 (reverse rands))]
                   ;; push the closure values
                   [state (foldl (lambda (v state)
                                   (state-push state v))
                                 state 
                                 (reverse closure-values))])
                   
                   
                   ;; evaluate the body
                   (let ([new-state 
                          (evaluate-at-expression-position body state)])
                     ;; Pop off values from the stack.
                     (state-popn new-state (+ (length rands)
                                              (length closure-values)))))])]
    
    ;; Otherwise, if it's a primitive, call out to the primitive.
    ;; Primitive procedures will just look at the state and the list of rands.
    [(? procedure?)
     #;(printf "Calling primitive procedure ~s, with state ~s and rands ~s~n" 
             rator state rands)
     #;(newline)
     (rator rands state)]))



;; run-branch: branch state -> state
(define (run-branch a-branch state)
  (match a-branch
    [(struct branch (test then else))
     state
     ;; FIXME
     #;(append (match test
                 [(? expr?)
                  (run-expr test)]
                 [(? seq?)
                  (run-seq test)]
                 [(? indirect?)
                  (run-indirect test)]
                 [else
                  (list)])
               (match then
                 [(? expr?)
                  (run-expr then)]
                 [(? seq?)
                  (run-seq then)]
                 [(? indirect?)
                  (run-indirect then)]
                 [else
                  (list)])
               (match else
                 [(? expr?)
                  (run-expr else)]
                 [(? seq?)
                  (run-seq else)]
                 [(? indirect?)
                  (run-indirect else)]
                 [else
                  (list)]))]))



;; run-with-cont-mark: with-cont-mark state -> state
(define (run-with-cont-mark a-with-cont-mark state)
  (match a-with-cont-mark
    [(struct with-cont-mark (key val body))
     state
     ;; FIXME
     #;(append (match key
                 [(? expr?)
                  (run-expr key)]
                 [(? seq?)
                  (run-seq key)]
                 [(? indirect?)
                  (run-indirect key)]
                 [else
                  (list)])
               (match val
                 [(? expr?)
                  (run-expr val)]
                 [(? seq?)
                  (run-seq val)]
                 [(? indirect?)
                  (run-indirect val)]
                 [else
                  (list)])
               (match body
                 [(? expr?)
                  (run-expr body)]
                 [(? seq?)
                  (run-seq body)]
                 [(? indirect?)
                  (run-indirect body)]
                 [else
                  (list)]))]))



;; run-beg0: beg0 state -> state
(define (run-beg0 a-beg0 state)
  (match a-beg0
    [(struct beg0 (seq))
     state
     #;(apply append (map (lambda (s)
                            (match s
                              [(? expr?)
                               (run-expr s)]
                              [(? seq?)
                               (run-seq s)]
                              [(? indirect?)
                               (run-indirect s)]
                              [else
                               (list)]))
                          seq))]))


;; run-varref: varref state -> state
(define (run-varref a-varref state)
  (match a-varref
    [(struct varref (toplevel))
     state
     ;; FIXME
     #;(run-toplevel toplevel)]))


;; run-assign: assign state -> state
(define (run-assign an-assign state)
  (match an-assign
    [(struct assign (id rhs undef-ok))
     state
     ;; FIXME
     #;(match rhs
         [(? expr?)
          (run-expr rhs)]
         [(? seq?)
          (run-seq rhs)]
         [(? indirect?)
          (run-indirect rhs)]
         [else
          (list)])]))


;; run-apply-values: apply-values state -> state
(define (run-apply-values an-apply-values state)
  (match an-apply-values
    [(struct apply-values (proc args-expr))
     ;; FIXME: check what kind of thing is the rator.
     ;; I expect it to be a closure-value when it has been run.
     ;; FIXME: evaluate each rand and plug its value into the argument list
     (let* ([state-with-args
             (evaluate-at-expression-position args-expr state)]
            [args (state-retvals state-with-args)]
            [state-with-operator-thunk
             (evaluate-at-expression-position proc state-with-args)]
            [operator-thunk (state-retval state-with-operator-thunk)])
       (apply-operator operator-thunk 
                       args 
                       state-with-operator-thunk))]))


;; run-primval: primval state -> state
(define (run-primval a-primval state)
  (match a-primval
    [(struct primval (id))
     (update-state-retval state (lookup-primitive id))]))


;; run-seq: seq state -> state
(define (run-seq a-seq state)
  (match a-seq
    [(struct seq (forms))
     (foldl (lambda (f state)
              (evaluate-at-expression-position f state))
            state
            forms)]))







;; Code is copied-and-pasted from compiler/decompile.
(define primitive-table
  ;; Figure out number-to-id mapping for kernel functions in `primitive'
  (let ([bindings
         (let ([ns (make-base-empty-namespace)])
           (parameterize ([current-namespace ns])
             (namespace-require ''#%kernel)
             (namespace-require ''#%unsafe)
             (namespace-require ''#%flfxnum)
             (for/list ([l (namespace-mapped-symbols)])
               (cons l (with-handlers ([exn:fail? (lambda (x) #f)])
                         (compile l))))))]
        [table (make-hash)])
    (for ([b (in-list bindings)])
      (let ([v (and (cdr b)
                    (zo-parse (let-values ([(in out) (make-pipe)])
                                (write (cdr b) out)
                                (close-output-port out)
                                in)))])
        (let ([n (match v
                   [(struct compilation-top (_ prefix (struct primval (n)))) n]
                   [else #f])])
          (hash-set! table n (car b)))))
    table))


;; This is completely wrong so far...
(define (lookup-primitive id)
  (let ([name (hash-ref primitive-table id)])
    #;(printf "Trying to get primitive ~s~n" name)
    (case name
      [(current-print)
       (lambda (args state)
         (let ([p (current-print)])
           #;(printf "I'm in current-print~n")
           (update-state-retval 
            state
            (lambda (args state)
              (printf "I'm in print, with args=~s~n" args)
              #;(printf "The state is ~s~n" state)
              (update-state-retval state (p (first args)))))))]
      [(apply)
       (lambda (args state)
         #;(printf "I'm in apply~n")
         (update-state-retval 
          state
          (apply-operator (first args) (rest args) state)))]
      
      [(values)
       (lambda (args state)
         #;(printf "I'm in values~n")
         (update-state-retval state args))]
      
      [(for-each)
        (lambda (args state)
          #;(printf "I'm in for-each, with state=~s and args=~s~n" state args)
          (let ([proc (first args)]
                [lists (rest args)])
            (foldl 
             (lambda (list-elts state)
               (apply-operator proc list-elts state))
             state
             lists)))]
 
      [else
       (error 'lookup-primitive (format "~s not implemented yet" id))])))










;; test: path -> state
;; exercising function
(define (test path)
  (let ([parsed (zo-parse (open-input-file path))])
    (run parsed fresh-state)
    (void)))

(test "../sandbox/42/compiled/42_ss_merged_ss.zo")
(test "../sandbox/square/compiled/square_ss_merged_ss.zo")