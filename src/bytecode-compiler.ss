#lang scheme/base

(require scheme/match
         scheme/contract
         compiler/zo-parse
         "jsexp.ss")


(provide/contract [compile-top (compilation-top? . -> . any/c)])


;; The structure of the code follows the type definitions in:
;; http://docs.plt-scheme.org/mzc/decompile.html?q=zo-parse#(def._((lib._compiler/zo-parse..ss)._indirect~3f))


;; Global parameters.
;; seen-indirects: maps the closures's symbolic identifiers to lambdas.
(define seen-indirects (make-parameter (make-hasheq)))



;; compile-top: top -> jsexp
(define (compile-top a-top)
  (parameterize ([seen-indirects (make-hasheq)])
    (match a-top
      [(struct compilation-top (max-let-depth prefix code))
       (let* ([compiled-code (compile-code code)]
              ;; WARNING: Order dependent!  We need compile-code to run first
              ;; since it initializes the seen-indirects parameter.
              [compiled-indirects (emit-indirects)])
         (void)
         (make-ht 'compilation-top
                  `((max-let-depth ,(make-int max-let-depth))
                    (prefix ,(compile-prefix prefix))
                    (compiled-indirects ,compiled-indirects)
                    (code ,compiled-code))))])))


;; emit-indirects: -> jsexp
;; Writes out all the indirect lambdas that we've seen.
(define (emit-indirects)
  (let ([ht (seen-indirects)])
    (make-vec 
     (for/list ([id+lam (in-hash-pairs ht)])
       (make-ht 'labeled-indirect 
                `((id ,(make-lit (car id+lam)))
                  (lam ,(compile-lam (cdr id+lam)))))))))


;; compile-prefix: prefix -> jsexp
(define (compile-prefix a-prefix)
  (match a-prefix
    [(struct prefix (num-lifts toplevels stxs))
     ;; FIXME: handle stxs?
     (make-ht 'prefix 
              `((num-lifts ,(make-int num-lifts))
                (toplevels ,(compile-toplevels toplevels))
                (stxs ,(compile-stxs stxs))))]))


;; compile-toplevels: (listof (or/c #f symbol? global-bucket? module-variable?)) -> jsexp
(define (compile-toplevels toplevels)
  (make-vec (map (lambda (a-toplevel)
                   (cond
                     [(eq? a-toplevel #f) 
                      (make-lit #f)]
                     [(symbol? a-toplevel)
                      (make-lit a-toplevel)]
                     [(global-bucket? a-toplevel) 
                      (make-ht 'global-bucket 
                               `((value ,(make-lit (global-bucket-name a-toplevel)))))]
                     [(module-variable? a-toplevel) 
                      (make-ht 'module-variable 
                               `((value ,(make-lit (module-variable-sym a-toplevel)))))]))
                 toplevels)))

;; compile-stxs: (listof stx) -> jsexp
(define (compile-stxs stxs)
  (make-vec (map (lambda (a-stx)
                   ;; FIXME: not right.  We need to translate
                   ;; stxs to runtime values eventually to support
                   ;; topsyntax
                   (make-lit (format "~s" stxs)))
                 stxs)))


;; compile-code: code -> jsexp
(define (compile-code a-code)
  (match a-code
    [(? form?)
     (compile-form a-code)]
    [(? indirect?)
     (compile-indirect a-code)]
    [else
     ;; literal value is self-evaluating
     (compile-constant a-code)]))


;; compile-constant: datum -> jsexp
(define (compile-constant a-constant)
  (make-ht 'constant 
           `((value ,(make-lit a-constant)))))



;; compile-form: form -> jsexp
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


;; compile-mod: mod -> jsexp
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
     (make-ht 'mod `((name ,(make-lit name))
                     (prefix ,(compile-prefix prefix))
                     (body ,(make-vec (map (lambda (b)
                                             (match b 
                                               [(? form?)
                                                (compile-form b)]
                                               [(? indirect?)
                                                (compile-indirect b)]
                                               [else
                                                (compile-constant b)]))
                                           body)))))]))

;; compile-splice: splice -> jsexp
(define (compile-splice a-splice)
  (match a-splice
    [(struct splice (forms))
     (make-ht 'splice `((value
                         ,(make-vec (map (lambda (f)
                                           (match f
                                             [(? form?)
                                              (compile-form f)]
                                             [(? indirect?)
                                              (compile-indirect f)]
                                             [else
                                              (compile-constant f)]))
                                         forms)))))]))




;; compile-at-expression-position: (U expr seq indirect any) -> jsexp
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



;; compile-def-values: def-values -> jsexp
;; Accumulates the values for rhs, and then installs each value in turn
;; into the toplevel.
(define (compile-def-values a-def-values)
  (match a-def-values
    [(struct def-values (ids rhs))
     (make-ht 'def-values 
              `((ids ,(make-vec 
                       (map compile-toplevel ids)))
                (body ,(compile-at-expression-position rhs))))]))


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


;; compile-expr: expr -> jsexp
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
     (compile-primval an-expr)]
    [(? branch?)
     (compile-branch an-expr)]
    [(? closure?)
     (compile-closure an-expr)]
    [(? beg0?)
     (compile-beg0 an-expr)]
    [(? with-cont-mark?)
     (compile-with-cont-mark an-expr)]))


;; run-lam: lam -> jsexp
(define (compile-lam a-lam)
  (match a-lam
    [(struct lam (name flags num-params param-types 
                       rest? closure-map closure-types 
                       max-let-depth body))
     (make-ht 'lam `((flags ,(make-vec (map make-lit flags)))
                     (num-params ,(make-int num-params))
                     (param-types ,(make-vec (map make-lit param-types)))
                     (rest? ,(make-lit rest?))
                     (closure-map ,(make-vec (map make-lit (vector->list closure-map))))
                     (closure-types ,(make-vec (map make-lit closure-types)))
                     (max-let-depth ,(make-int max-let-depth))
                     (body ,(compile-at-expression-position body))))]))




;; compile-closure: closure -> jsexp
(define (compile-closure a-closure)
  (match a-closure 
    [(struct closure (lam gen-id))
     (begin
       (hash-set! (seen-indirects) gen-id lam)
       (make-ht 'closure `((lam ,(compile-lam lam))
                           (gen-id ,(make-lit gen-id)))))]))


;; compile-indirect: indirect -> jsexp
(define (compile-indirect an-indirect)
  (match an-indirect
    [(struct indirect ((struct closure (lam gen-id))))
     (begin
       ;; Keep track of the indirect.  We'll need to generate the s-expression for it in a moment
       (hash-set! (seen-indirects) gen-id lam)
       (make-ht 'indirect `((value ,(make-lit gen-id)))))]))



;; compile-localref: localref -> jsexp
(define (compile-localref a-localref)
  (match a-localref
    [(struct localref (unbox? pos clear? other-clears? flonum?))
     (make-ht 'localref `((unbox? ,(make-lit unbox?))
                          (pos ,(make-int pos))
                          (clear ,(make-lit clear?))
                          (other-clears? ,(make-lit other-clears?))
                          (flonum? ,(make-lit flonum?))))]))


;; compile-toplevel: toplevel -> jsexp
(define (compile-toplevel a-toplevel)
  (match a-toplevel
    [(struct toplevel (depth pos const? ready?))
     (make-ht 'toplevel `((depth ,(make-int depth))
                          (pos ,(make-int pos))
                          (const? ,(make-lit const?))
                          (ready? ,(make-lit ready?))))]))


;; compile-application: application -> jsexp
(define (compile-application an-application)
  (match an-application
    [(struct application (rator rands))
     (make-ht 'application 
              `((rator ,(compile-at-expression-position rator))
                (rands ,(make-vec (map compile-at-expression-position rands)))))]))


;; compile-apply-values: apply-values -> jsexp
(define (compile-apply-values an-apply-values)
  (match an-apply-values
    [(struct apply-values (proc args-expr))
     (make-ht 'apply-values 
              `((proc ,(compile-at-expression-position proc))
                (args-expr ,(compile-at-expression-position args-expr))))]))


;; compile-primval: primval jsexp -> jsexp
(define (compile-primval a-primval)
  (match a-primval
    [(struct primval (id))
     (make-ht 'primval `((value ,(make-lit 
                                  (symbol->string (hash-ref primitive-table id))))))]))


;; compile-branch: branch -> jsexp
(define (compile-branch a-branch)
  (match a-branch
    [(struct branch (test then else))
     (make-ht 'branch `((test ,(compile-at-expression-position test))
			(then ,(compile-at-expression-position then))
			(else ,(compile-at-expression-position else))))]))


;; compile-seq: seq -> jsexp
(define (compile-seq a-seq)
  (match a-seq
    [(struct seq (forms))
     (make-ht 'seq 
              `((forms 
                 ,(make-vec 
                   (map compile-at-expression-position forms)))))]))


;; compile-beg0: seq -> jsexp
(define (compile-beg0 a-beg0)
  (match a-beg0
    [(struct beg0 (seq))
     (make-ht 'beg0 
              `((seq 
                 ,(make-vec 
                   (map compile-at-expression-position seq)))))]))


(define (compile-with-cont-mark a-with-cont-mark)
  (match a-with-cont-mark
    [(struct with-cont-mark (key val body))
     (make-ht 'with-cont-mark1 
              `((key ,(compile-at-expression-position key))
                (val ,(compile-at-expression-position val))
                (body ,(compile-at-expression-position body))))]))

             



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



#;(test "../tests/42/compiled/42_ss_merged_ss.zo")
#;(test "../tests/square/compiled/square_ss_merged_ss.zo")