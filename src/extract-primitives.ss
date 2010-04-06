#lang scheme/base

(require scheme/match
         compiler/zo-parse)



(define fixme 'fixme)


;; Let's write a program to figure out all the primitives used in a compilation-top.

;; The structure of the code follows the type definitions in:
;; http://docs.plt-scheme.org/mzc/decompile.html?q=zo-parse#(def._((lib._compiler/zo-parse..ss)._indirect~3f))


;; extract-primitives: toplevel -> (listof symbol)
(define (extract-primitives a-top)
  (match a-top
    [(struct compilation-top (max-let-depth prefix code))
     (list (extract-prefix prefix)
           (extract-code code))]))


(define (extract-prefix a-prefix)
  (match a-prefix
    [(struct prefix (num-lifts toplevels stxs))
     fixme]))


;; A code is either a form, an indirect, or a literal value.
(define (extract-code a-code)
  (match a-code
    [(struct form ())
     fixme]
    [(struct indirect (v))
     ;; v is a closure
     fixme]
    [else
     ;; literal value
     fixme]))

(define (extract-form a-form)
  (match a-form
    [(struct def-values (ids rhs))
     fixme]
    [(struct def-syntaxes (ids rhs prefix max-let-depth))
     fixme]
    [(struct def-for-syntax (ids rhs prefix max-let-depth))
     fixme]
    [(struct req (reqs dummy))
     fixme]
    [(struct seq (forms))
     fixme]
    [(struct splice (forms))
     fixme]
    [(struct mod (name self-modidx prefix provides requires body syntax-body unexported
                       max-let-depth dummy lang-info internal-context))
     fixme]
    [(struct expr)
     fixme]))



(define (extract-provided a-provided)
  (match a-provided
    [(struct provided (name src src-name nom-mod src-phase protected? insp))
     fixme]))


(define (extract-expr an-expr)
  (match an-expr
    [(struct lam (name flags num-params param-types rest? closure-map closure-types max-let-depth body))
     fixme]
    [(struct closure (code gen-id))
     fixme]
    [(struct indirect (v))
     fixme]
    [(struct case-lam (name clauses))
     fixme]
    [(struct let-one (rhs body flonum?))
     fixme]
    [(struct let-void (count boxes? body))
     fixme]
    [(struct install-value (count pos boxes? rhs body))
     fixme]
    [(struct let-rec (procs body))
     fixme]
    [(struct box-env (pos body))
     fixme]
    [(struct localref (unbox? pos clear? other-clears? flonum?))
     fixme]
    [(struct toplevel (depth pos const? ready?))
     fixme]
    [(struct topsyntax (depth pos midpt))
     fixme]
    [(struct application (rator rands))
     fixme]
    [(struct branch (test then else))
     fixme]
    [(struct with-cont-mark (key val body))
     fixme]
    [(struct beg0 (seq))
     fixme]
    [(struct varref (toplevel))
     fixme]
    [(struct assign (id rhs undef-ok?))
     fixme]
    [(struct apply-values (proc ars-expr))
     fixme]
    [(struct primval (id))
     fixme
     #;(list id)
     ]))
             




(define (test)
  (define flight-lander-parsing
    (zo-parse (open-input-file "flight-lander_ss_merged_ss.zo")))
  (extract-primitives flight-lander-parsing))
