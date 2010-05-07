#lang scheme/base

(require scheme/match
         scheme/list
         scheme/contract
         compiler/zo-parse
         "sexp.ss")


(provide/contract [compile-top (compilation-top? . -> . any/c)])


;; The structure of the code follows the type definitions in:
;; http://docs.plt-scheme.org/mzc/decompile.html?q=zo-parse#(def._((lib._compiler/zo-parse..ss)._indirect~3f))


;; seen-indirects: maps the closures to unique identifiers
(define seen-indirects (make-hasheq))


;; compile-top: top -> sexp
(define (compile-top a-top)
  (match a-top
    [(struct compilation-top (max-let-depth prefix code))
     `(compilation-top ,max-let-depth ,(compile-prefix prefix) ,(compile-code code))]))


;; compile-prefix: prefix -> sexp
(define (compile-prefix a-prefix)
  (match a-prefix
    [(struct prefix (num-lifts toplevels stxs))
     ;; FIXME: handle stxs?
     `(prefix ,num-lifts ,(compile-toplevels toplevels))]))


(define (compile-toplevels toplevels)
  (map (lambda (a-toplevel)
         (cond
           [(eq? a-toplevel #f) #f]
           [(symbol? a-toplevel) a-toplevel]
           [(global-bucket? a-toplevel) `(global-bucket ,(global-bucket-name a-toplevel))]
           [(module-variable? a-toplevel) `(module-variable ,(module-variable-sym a-toplevel))]))
       toplevels))


;; compile-code: code -> sexp
(define (compile-code a-code)
  (match a-code
    [(? form?)
     (compile-form a-code)]
    [(? indirect?)
     (compile-indirect a-code)]
    [else
     ;; literal value is self-evaluating
     (compile-constant a-code)]))


;; compile-constant: datum -> sexp
(define (compile-constant a-constant)
  `(constant ,a-constant))



;; compile-form: form -> sexp
(define (compile-form a-form)
  (match a-form
    [(? def-values?)
     (compile-def-values a-form)]
    [(? seq?)
     (compile-seq a-form)]
    [(? splice?)
     (compile-splice a-form)]
    [(? mod?)
     (compile-mod a-form)]
    [(? expr?)
     (compile-expr a-form)]))


;; compile-mod: mod -> sexp
(define (compile-mod a-mod)
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
     `(mod ,name 
           ,(compile-prefix prefix) 
           ,(map (lambda (b)
                  (match b 
                    [(? form?)
                     (compile-form b)]
                    [(? indirect?)
                     (compile-indirect b)]
                    [else
                     (compile-constant b)]))
                body))]))
  
  

;; compile-splice: splice -> s-exp
(define (compile-splice a-splice)
  (match a-splice
    [(struct splice (forms))
     `(splice ,(map (lambda (f)
                      (match f
                        [(? form?)
                         (compile-form f)]
                        [(? indirect?)
                         (compile-indirect f)]
                        [else
                         (compile-constant f)]))
                    forms))]))
  



;; compile-at-expression-position: (U expr seq indirect any) icode -> icode
;;
;; evaluate the expression-like thing at x, installing it into the retvals of
;; the current state.
(define (compile-at-expression-position x)
  (match x
    [(? expr?)
     (compile-expr x)]
    [(? seq?)
     (compile-seq x)]
    [(? indirect?)
     (compile-indirect x)]
    [else
     (compile-constant x)]))
 


;; compile-def-values: def-values icode -> icode
;; Accumulates the values for rhs, and then installs each value in turn
;; into the toplevel.
(define (compile-def-values a-def-values)
  (match a-def-values
    [(struct def-values (ids rhs))
     `(def-values 
        ,(map (lambda (an-id)
                (match an-id
                  [(struct toplevel (depth pos const? ready?))
                   `(toplevel ,depth ,pos ,const? ,ready?)]))
              ids)
        ,(compile-at-expression-position rhs))]))


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



(define (compile-expr an-expr)
  (match an-expr
    [(? lam?)
     (compile-lam an-expr)]
    [(? localref?)
     (compile-localref an-expr)]
    [(? toplevel?)
     (compile-toplevel an-expr)]
    [(? application?)
     (compile-application an-expr)]
    [(? apply-values?)
     (compile-apply-values an-expr)]
    [(? primval?)
     (compile-primval an-expr)]))


;; run-lam: lam s-exp -> s-exp
(define (compile-lam a-lam)
  (match a-lam
    [(struct lam (name flags num-params param-types 
                       rest? closure-map closure-types 
                       max-let-depth body))
     `(lam ,flags ,num-params ,param-types ,rest? ,closure-map ,closure-types ,max-let-depth 
           ,(compile-at-expression-position body))]))



;; compile-closure: closure s-exp -> s-exp
(define (compile-closure a-closure)
  (match a-closure 
    [(struct closure (lam gen-id))
     `(closure ,(compile-lam lam) ,gen-id)]))



;; compile-indirect: indirect s-exp -> s-exp
(define (compile-indirect an-indirect)
  (match an-indirect
    [(struct indirect ((struct closure (lam gen-id))))
     `(indirect ,gen-id)]))




;; compile-localref: localref -> s-exp
(define (compile-localref a-localref)
  (match a-localref
    [(struct localref (unbox? pos clear? other-clears? flonum?))
     `(localref ,unbox? ,pos ,clear? ,other-clears? ,flonum?)]))



;; compile-toplevel: toplevel -> s-exp
(define (compile-toplevel a-toplevel)
  (match a-toplevel
    [(struct toplevel (depth pos const? ready?))
     `(toplevel ,depth ,pos ,const? ,ready?)]))




;; compile-application: application icode -> icode
(define (compile-application an-application)
  (match an-application
    [(struct application (rator rands))
     `(application ,(compile-at-expression-position rator)
                   ,(compile-at-expression-position rands))]))


;; compile-apply-values: apply-values icode -> icode
(define (compile-apply-values an-apply-values)
  (match an-apply-values
    [(struct apply-values (proc args-expr))
     `(apply-values ,(compile-at-expression-position proc)
                    ,(compile-at-expression-position args-expr))]))


;; compile-primval: primval icode -> icode
(define (compile-primval a-primval)
  (match a-primval
    [(struct primval (id))
     `(primval ,(hash-ref primitive-table id))]))


;; compile-seq: seq icode -> icode
(define (compile-seq a-seq)
  (match a-seq
    [(struct seq (forms))
     `(seq (map compile-at-expression-position forms))]))





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
               (cons l (with-handlers ([exn:fail? (lambda (x) 
                                                    #f)])
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




;; test: path -> state
;; exercising function
(define (test path)
  (let ([parsed (zo-parse (open-input-file path))])
    (compile-top parsed)))



#;(test "../sandbox/42/compiled/42_ss_merged_ss.zo")
#;(test "../sandbox/square/compiled/square_ss_merged_ss.zo")