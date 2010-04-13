#lang scheme/base

(require scheme/match
         compiler/zo-parse
         "state.ss"
         "primitive-table.ss")



;; Let's write a program to figure out all the primitives used in a compilation-top.

;; The structure of the code follows the type definitions in:
;; http://docs.plt-scheme.org/mzc/decompile.html?q=zo-parse#(def._((lib._compiler/zo-parse..ss)._indirect~3f))





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
     (update-state-retval a-code)]))



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
     ;; FIXME: handle the variables here that we're not
     ;; looking at, like prefix and max-let-depth.
     (foldl (lambda (b state)
              (match b 
                [(? form?)
                 (run-form b state)]
                [(? indirect?)
                 (run-indirect b state)]
                [else
                 (update-state-retval b)]))
            state
            body)

     ;; FIXME: do something with the syntax body.
     #;(foldl (lambda (b)
              (match b
                [(? def-syntaxes?)
                 (run-def-syntaxes b)]
                [(? def-for-syntax?)
                 (run-def-for-syntax b)]))
            syntax-body)]))


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


;; run-def-values: def-values state -> state
(define (run-def-values a-def-values state)
  (match a-def-values
    [(struct def-values (ids rhs))
     (match rhs
       [(? expr?)
        (run-expr rhs state)]
       [(? seq?)
        (run-seq rhs state)]
       [(? indirect?)
        (run-indirect rhs state)]
       [else
        ;; FIXME!
        state
        ;; literal value
        #;(list)])]))



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
     ;; FIXME
     state
     #;(match body
       [(? expr?)
        (run-expr body state)]
       [(? seq?)
        (run-seq body state)]
       [(? indirect?)
        (run-indirect body state)]
       [else
        ;; it's a literal datum
        (list)])]))


;; run-closure: closure state -> state
(define (run-closure a-closure state)
  state
  ;; FIXME
  #;(match a-closure 
    [(struct closure (lam gen-id))
     (run-lam lam)]))


;; run-indirect: indirect state -> state
(define (run-indirect an-indirect state)
  state
  ;; FIXME
  #;(match an-indirect
    [(struct indirect (v))
     (cond [(hash-ref visit-ht v #f)
            (hash-set! visit-ht v #t)
            (run-closure v)]
           [else
            (list)])]))


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
     state
     ;; FIXME
     #;(list)]))



;; run-toplevel: toplevel state -> state
(define (run-toplevel a-toplevel state)
  (match a-toplevel
    [(struct toplevel (depth pos const? ready?))
     state
     #;(list)]))


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
     state
     ;; FIXME
     #;(append (match rator
               [(? expr?)
                (run-expr rator)]
               [(? seq?)
                (run-seq rator)]
               [(? indirect?)
                (run-indirect rator)]
               [else
                (list)])
             (apply append (map (lambda (r)
                                  (match r
                                    [(? expr?)
                                     (run-expr r)]
                                    [(? seq?)
                                     (run-seq r)]
                                    [(? indirect?)
                                     (run-indirect r)]
                                    [else
                                     (list)]))
                                rands)))]))


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
     state
     ;; FIXME
     #;(append (match proc
               [(? expr?)
                (run-expr proc)]
               [(? seq?)
                (run-seq proc)]
               [(? indirect?)
                (run-indirect proc)]
               [else
                (list)])
             (match args-expr
               [(? expr?)
                (run-expr args-expr)]
               [(? seq?)
                (run-seq args-expr)]
               [(? indirect?)
                (run-indirect args-expr)]
               [else
                (list)]))]))


;; run-primval: primval state -> state
(define (run-primval a-primval state)
  (match a-primval
    [(struct primval (id))
     state
     ;; FIXME
     #;(list (hash-ref primitive-table id))]))




;; run-seq: seq state -> state
(define (run-seq a-seq state)
  (match a-seq
    [(struct seq (forms))
     (foldl (lambda (f state)
              state
              ;; FIXME
              #;(match f
                  [(? form?)
                   (run-form f)]
                  [(? indirect?)
                   (run-indirect f)]
                  [else
                   ;; it's a literal datum
                   (list)]))
            state
            forms)]))

     







;; test: path -> state
;; exercising function
(define (test path)
  (run (zo-parse (open-input-file path))
       (make-state (void))))

(test "../sandbox/42/compiled/42_ss_merged_ss.zo")