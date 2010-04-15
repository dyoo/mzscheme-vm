#lang scheme/base

(require scheme/match
         scheme/list
         compiler/zo-parse
         "structs.ss"
         "icode.ss")

;; The structure of the code follows the type definitions in:
;; http://docs.plt-scheme.org/mzc/decompile.html?q=zo-parse#(def._((lib._compiler/zo-parse..ss)._indirect~3f))




;; compile: top icode -> icode
(define (compile a-top next)
  (compile-top a-top next))


;; compile-top: top icode -> icode
(define (compile-top a-top next)
  (match a-top
    [(struct compilation-top (max-let-depth prefix code))
     (compile-max-let-depth max-let-depth
                            (compile-prefix prefix
                                            (compile-code code next)))]))


;; compile-max-let-depth: exact-nonnegative-integer icode -> icode
(define (compile-max-let-depth max-let-depth next)
  (make-icode:max-let-depth max-let-depth next))


;; compile-prefix: prefix icode -> icode
(define (compile-prefix a-prefix next)
  (match a-prefix
    [(struct prefix (num-lifts toplevels stxs))
     (make-icode:prefix num-lifts toplevels stxs next)]))


;; compile-code: code icode -> icode
(define (compile-code a-code next)
  (match a-code
    [(? form?)
     (compile-form a-code next)]
    #;[(? indirect?)
     (compile-indirect a-code next)]
    [else
     ;; literal value is self-evaluating
     (compile-constant a-code next)]))


;; compile-constant: datum icode -> icode
(define (compile-constant a-constant next)
  (make-icode:constant a-constant next))



;; compile-form: form state -> state
(define (compile-form a-form next)
  (match a-form
    [(? def-values?)
     (compile-def-values a-form next)]
    #;[(? def-syntaxes?)
     (run-def-syntaxes a-form state)]
    #;[(? def-for-syntax?)
     (run-def-for-syntax a-form state)]
    #;[(? req?)
     (run-req a-form state)]
    [(? seq?)
     (compile-seq a-form next)]
    [(? splice?)
     (compile-splice a-form next)]
    [(? mod?)
     (compile-mod a-form next)]
    [(? expr?)
     (compile-expr a-form next)]))


;; compile-mod: mod icode -> icode
(define (compile-mod a-mod next)
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
     (compile-prefix prefix
                     (compile-max-let-depth max-let-depth
                                            (foldr (lambda (b next)
                                                     (match b 
                                                       [(? form?)
                                                        (compile-form b next)]
                                                       #;[(? indirect?)
                                                        (compile-indirect b next)]
                                                       [else
                                                        (compile-constant b next)]))
                                                   next
                                                   body)))]))



;; run-splice: splice icode -> icode
(define (compile-splice a-splice next)
  (match a-splice
    [(struct splice (forms))
     (foldr (lambda (f next)
              (match f
                [(? form?)
                 (compile-form f next)]
                #;[(? indirect?)
                 (compile-indirect f next)]
                [else
                 (compile-constant f next)]))
            next
            forms)]))


;; run-req: req state -> state
#;(define (run-req a-req state)
  (match a-req 
    [(struct req (reqs dummy))
     state
     ;; FIXME
     #;(list)]))


;; compile-at-expression-position: (U expr seq indirect any) icode -> icode
;;
;; evaluate the expression-like thing at x, installing it into the retvals of
;; the current state.
(define (compile-at-expression-position x next)
  (match x
    [(? expr?)
     (compile-expr x next)]
    [(? seq?)
     (compile-seq x next)]
    #;[(? indirect?)
     (run-indirect x state)]
    [else
     (compile-constant x next)]))
 


;; compile-def-values: def-values icode -> icode
;; Accumulates the values for rhs, and then installs each value in turn
;; into the toplevel.
(define (compile-def-values a-def-values next)
  (match a-def-values
    [(struct def-values (ids rhs))
     (compile-at-expression-position rhs
                                     (foldr (lambda (an-id next)
                                              (match an-id
                                                [(struct toplevel (depth pos const? ready?))
                                                 (make-icode:set-toplevel depth pos next)]))
                                            next
                                            ids))]))


#;(define (run-def-syntaxes a-def-syntaxes state)
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

#;(define (run-def-for-syntax a-def-for-syntax state)
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



(define (compile-expr an-expr next)
  (match an-expr
    [(? lam?)
     (compile-lam an-expr next)]
    #;[(? closure?)
       (compile-closure an-expr state)]
    #;[(? indirect?)
       (run-indirect an-expr state)]
    #;[(? case-lam?)
       (run-case-lam an-expr state)]
    #;[(? let-one?)
       (run-let-one an-expr state)]
    #;[(? let-void?)
       (run-let-void an-expr state)]
    #;[(? install-value?)
       (run-install-value an-expr state)]
    #;[(? let-rec?)
       (run-let-rec an-expr state)]
    #;[(? boxenv?)
       (run-boxenv an-expr state)]
    [(? localref?)
     (compile-localref an-expr next)]
    #;[(? toplevel?)
       (run-toplevel an-expr state)]
    #;[(? topsyntax?)
       (run-topsyntax an-expr state)]
    [(? application?)
     (compile-application an-expr next)]
    #;[(? branch?)
     (compile-branch an-expr next)]
    #;[(? with-cont-mark?)
     (run-with-cont-mark an-expr state)]
    #;[(? beg0?)
     (run-beg0 an-expr state)]
    #;[(? varref?)
     (run-varref an-expr state)]
    #;[(? assign?)
     (run-assign an-expr state)]
    [(? apply-values?)
     (compile-apply-values an-expr next)]
    [(? primval?)
     (compile-primval an-expr next)]))


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
#;(define (run-indirect an-indirect state)
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



;; compile-localref: localref next -> next
(define (compile-localref a-localref state)
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


;; compile-application: application state -> state
(define (compile-application an-application state)
  (match an-application
    [(struct application (rator rands))
     (make-icode:frame 
      (foldr (lambda (rand next)
               (compile-at-expression-position rand 
                                               (make-icode:argument next)))
             (compile-at-expression-position rator 
                                             (make-icode:apply next))
             rands))]))


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




;; compile-branch: branch test -> test
#;(define (compile-branch a-branch state)
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
#;(define (run-with-cont-mark a-with-cont-mark state)
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
#;(define (run-beg0 a-beg0 state)
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
#;(define (run-varref a-varref state)
  (match a-varref
    [(struct varref (toplevel))
     state
     ;; FIXME
     #;(run-toplevel toplevel)]))


;; run-assign: assign state -> state
#;(define (run-assign an-assign state)
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


;; compile-apply-values: apply-values icode -> icode
(define (compile-apply-values an-apply-values next)
  (match an-apply-values
    [(struct apply-values (proc args-expr))
     (make-icode:frame
      (compile-at-expression-position 
       args-expr
       (make-icode:argument
        (compile-at-expression-position 
         proc
         (make-icode:apply 
          (make-icode:return 1 next))))))]))


;; compile-primval: primval icode -> icode
(define (compile-primval a-primval next)
  (match a-primval
    [(struct primval (id))
     (make-icode:primitive (lookup-primitive id) next)]))



;; compile-seq: seq icode -> icode
(define (compile-seq a-seq next)
  (match a-seq
    [(struct seq (forms))
     (foldr (lambda (f state)
              (compile-at-expression-position f next))
            next
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

#;(test "../sandbox/42/compiled/42_ss_merged_ss.zo")
(test "../sandbox/square/compiled/square_ss_merged_ss.zo")