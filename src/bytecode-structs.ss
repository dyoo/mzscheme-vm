#lang scheme/base
(require scheme/contract
         scheme/list
         scheme/match
         (prefix-in internal: compiler/zo-structs))

#| Unresolved issues

  what are the booleans in lexical-rename?

  contracts that are probably too generous:
  prefix-stxs
  provided-nom-src
  lam-num-params
  lexical-rename-alist
  all-from-module

|#

;; ----------------------------------------
;;  Structures to represent bytecode

(define-syntax-rule (define-form-struct* id id+par ([field-id field-contract] ...))
  (begin
    (define-struct id+par (field-id ...) #:transparent)
    (provide/contract
     [struct id ([field-id field-contract] ...)])))

(define-syntax define-form-struct
  (syntax-rules ()
    [(_ (id sup) . rest)
     (define-form-struct* id (id sup) . rest)]
    [(_ id . rest)
     (define-form-struct* id id . rest)]))

;; In toplevels of resove prefix:
(define-form-struct global-bucket ([name symbol?])) ; top-level binding
(define-form-struct module-variable ([modidx module-path-index?] 
                                     [sym symbol?] 
                                     [pos exact-integer?] 
                                     [phase (or/c 0 1)])) ; direct access to exported id

;; Syntax object
(define-form-struct wrap ())
(define-form-struct wrapped ([datum any/c] 
                             [wraps (listof wrap?)] 
                             [certs (or/c list? #f)]))

;; In stxs of prefix:
(define-form-struct stx ([encoded wrapped?]))

(define-form-struct prefix ([num-lifts exact-nonnegative-integer?] 
                            [toplevels (listof (or/c #f symbol? global-bucket? module-variable?))] 
                            [stxs list?]))   ; should be (listof stx?) sets up top-level and syntax-object array

(define-form-struct form ())
(define-form-struct (expr form) ())

;; A static closure can refer directly to itself, creating a cycle
(define-struct indirect ([v #:mutable]) #:transparent)

(define-form-struct compilation-top ([max-let-depth exact-nonnegative-integer?] [prefix prefix?] [code (or/c form? indirect? any/c)])) ; compiled code always wrapped with this

;; A provided identifier
(define-form-struct provided ([name symbol?] 
                              [src (or/c module-path-index? #f)] 
                              [src-name symbol?] 
                              [nom-src any/c] ; should be (or/c module-path-index? #f)
                              [src-phase (or/c 0 1)] 
                              [protected? boolean?] 
                              [insp (or/c boolean? void?)]))

(define-form-struct (toplevel expr) ([depth exact-nonnegative-integer?] 
                                     [pos exact-nonnegative-integer?] 
                                     [const? boolean?] 
                                     [ready? boolean?]))  ; access binding via prefix array (which is on stack)

(define-form-struct (seq form) ([forms (listof (or/c form? indirect? any/c))])) ; `begin'

;; Definitions (top level or within module):
(define-form-struct (def-values form) ([ids (listof (or/c toplevel? symbol?))] ; added symbol?
                                       [rhs (or/c expr? seq? indirect? any/c)])) 
(define-form-struct (def-syntaxes form) ([ids (listof (or/c toplevel? symbol?))] ; added symbol?                                        
                                         [rhs (or/c expr? seq? indirect? any/c)] 
                                         [prefix prefix?] 
                                         [max-let-depth exact-nonnegative-integer?])) 
(define-form-struct (def-for-syntax form) ([ids (listof (or/c toplevel? symbol?))] ; added symbol?
                                           [rhs (or/c expr? seq? indirect? any/c)] 
                                           [prefix prefix?] 
                                           [max-let-depth exact-nonnegative-integer?])) 

(define-form-struct (mod form) ([name symbol?] 
                                [self-modidx module-path-index?] 
                                [prefix prefix?] 
                                [provides (listof (list/c (or/c exact-integer? #f)
                                                          (listof provided?)
                                                          (listof provided?)))] 
                                [requires (listof (cons/c (or/c exact-integer? #f)
                                                          (listof module-path-index?)))]
                                [body (listof (or/c form? indirect? any/c))] 
                                [syntax-body (listof (or/c def-syntaxes? def-for-syntax?))] 
                                [unexported (list/c (listof symbol?) (listof symbol?)
                                                    (listof symbol?))] 
                                [max-let-depth exact-nonnegative-integer?]
                                [dummy toplevel?]
                                [lang-info (or/c #f (vector/c module-path? symbol? any/c))]
                                [internal-context (or/c #f #t stx?)]))

(define-form-struct (lam expr) ([name (or/c symbol? vector? empty?)]
                                [flags (listof (or/c 'preserves-marks 'is-method 'single-result))]
                                [num-params integer?] ; should be exact-nonnegative-integer?
                                [param-types (listof (or/c 'val 'ref 'flonum))]
                                [rest? boolean?]
                                [closure-map (vectorof exact-nonnegative-integer?)]
                                [closure-types (listof (or/c 'val/ref 'flonum))]
                                [max-let-depth exact-nonnegative-integer?]
                                [body (or/c expr? seq? indirect? any/c)])) ; `lambda'
(define-form-struct (closure expr) ([code lam?] [gen-id symbol?])) ; a static closure (nothing to close over)
(define-form-struct (case-lam expr) ([name (or/c symbol? vector? empty?)] [clauses (listof (or/c lam? indirect?))])) ; each clause is a lam (added indirect)

(define-form-struct (let-one expr) ([rhs (or/c expr? seq? indirect? any/c)] [body (or/c expr? seq? indirect? any/c)] [flonum? boolean?])) ; pushes one value onto stack
(define-form-struct (let-void expr) ([count exact-nonnegative-integer?] [boxes? boolean?] [body (or/c expr? seq? indirect? any/c)])) ; create new stack slots
(define-form-struct (install-value expr) ([count exact-nonnegative-integer?] 
                                          [pos exact-nonnegative-integer?] 
                                          [boxes? boolean?] 
                                          [rhs (or/c expr? seq? indirect? any/c)] 
                                          [body (or/c expr? seq? indirect? any/c)])) ; set existing stack slot(s)
(define-form-struct (let-rec expr) ([procs (listof lam?)] [body (or/c expr? seq? indirect? any/c)])) ; put `letrec'-bound closures into existing stack slots
(define-form-struct (boxenv expr) ([pos exact-nonnegative-integer?] [body (or/c expr? seq? indirect? any/c)])) ; box existing stack element

(define-form-struct (localref expr) ([unbox? boolean?] [pos exact-nonnegative-integer?] [clear? boolean?] [other-clears? boolean?] [flonum? boolean?])) ; access local via stack


(define-form-struct (topsyntax expr) ([depth exact-nonnegative-integer?] [pos exact-nonnegative-integer?] [midpt exact-nonnegative-integer?])) ; access syntax object via prefix array (which is on stack)

(define-form-struct (application expr) ([rator (or/c expr? seq? indirect? any/c)] [rands (listof (or/c expr? seq? indirect? any/c))])) ; function call
(define-form-struct (branch expr) ([test (or/c expr? seq? indirect? any/c)] [then (or/c expr? seq? indirect? any/c)] [else (or/c expr? seq? indirect? any/c)])) ; `if'
(define-form-struct (with-cont-mark expr) ([key (or/c expr? seq? indirect? any/c)] 
                                           [val (or/c expr? seq? indirect? any/c)] 
                                           [body (or/c expr? seq? indirect? any/c)])) ; `with-continuation-mark'
(define-form-struct (beg0 expr) ([seq (listof (or/c expr? seq? indirect? any/c))])) ; `begin0'
(define-form-struct (splice form) ([forms (listof (or/c form? indirect? any/c))])) ; top-level `begin'
(define-form-struct (varref expr) ([toplevel toplevel?])) ; `#%variable-reference'
(define-form-struct (assign expr) ([id toplevel?] [rhs (or/c expr? seq? indirect? any/c)] [undef-ok? boolean?])) ; top-level or module-level set!
(define-form-struct (apply-values expr) ([proc (or/c expr? seq? indirect? any/c)] [args-expr (or/c expr? seq? indirect? any/c)])) ; `(call-with-values (lambda () ,args-expr) ,proc)
(define-form-struct (primval expr) ([id exact-nonnegative-integer?])) ; direct preference to a kernel primitive





;;;;;;;;;; dyoo


;; a world primitive
(define-form-struct (world-primval expr) ([id symbol?]))


;;;;;;;;;;;;;;;;


;; Top-level `require'
(define-form-struct (req form) ([reqs syntax?] [dummy toplevel?]))

(define-form-struct (lexical-rename wrap) ([bool1 boolean?] ; this needs a name
                                           [bool2 boolean?] ; this needs a name
                                           [alist any/c])) ; should be (listof (cons/c symbol? symbol?))
(define-form-struct (phase-shift wrap) ([amt exact-integer?] [src (or/c module-path-index? #f)] [dest (or/c module-path-index? #f)]))
(define-form-struct (wrap-mark wrap) ([val exact-integer?]))
(define-form-struct (prune wrap) ([sym any/c]))

(define-form-struct all-from-module ([path module-path-index?] 
                                     [phase (or/c exact-integer? #f)] 
                                     [src-phase any/c] ; should be (or/c exact-integer? #f)
                                     [exceptions list?] ; should be (listof symbol?)
                                     [prefix any/c])) ; should be (or/c symbol? #f)

(define-form-struct nominal-path ())
(define-form-struct (simple-nominal-path nominal-path) ([value module-path-index?]))
(define-form-struct (imported-nominal-path nominal-path) ([value module-path-index?] 
                                                          [import-phase exact-integer?]))
(define-form-struct (phased-nominal-path nominal-path) ([value module-path-index?]
                                                        [import-phase (or/c false/c exact-integer?)]
                                                        [phase exact-integer?]))

(define-form-struct module-binding ())
(define-form-struct (phased-module-binding module-binding) ([path module-path-index?]
                                                            [phase exact-integer?]
                                                            [export-name any/c]
                                                            [nominal-path nominal-path?]
                                                            [nominal-export-name any/c]))
(define-form-struct (exported-nominal-module-binding module-binding) ([path module-path-index?]
                                                                      [export-name any/c]
                                                                      [nominal-path nominal-path?]
                                                                      [nominal-export-name any/c]))
(define-form-struct (nominal-module-binding module-binding) ([path module-path-index?]
                                                             [nominal-path nominal-path?]))
(define-form-struct (exported-module-binding module-binding) ([path module-path-index?]
                                                              [export-name any/c]))
(define-form-struct (simple-module-binding module-binding) ([path module-path-index?]))

(define-form-struct (module-rename wrap) ([phase (or/c exact-integer? #f)] 
                                          [kind (or/c 'marked 'normal)] 
                                          [set-id any/c] 
                                          [unmarshals (listof all-from-module?)]
                                          [renames (listof (cons/c symbol? module-binding?))] 
                                          [mark-renames any/c] 
                                          [plus-kern? boolean?]))

(provide/contract (struct indirect ([v (or/c closure? #f)])))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Translation from mzscheme 4.2.5 bytecode structures to our own.


(define (translate-compilation-top a-top)
  (match a-top
    [(struct internal:compilation-top (max-let-depth prefix code))
     (make-compilation-top max-let-depth (translate-prefix prefix) (translate-code code))]))

(define (translate-code a-code)
  (match a-code
    [(? internal:form?)
     (translate-form a-code)]
    [(? internal:indirect?)
     (translate-indirect a-code)]
    [else
     a-code]))
     

(define (translate-prefix a-prefix)
  (match a-prefix
    [(struct internal:prefix (num-lifts toplevels stxs))
     (make-prefix num-lifts 
                  (map (lambda (x)
                         (match x
                           ['#f
                             #f]
                           [(? symbol?)
                            x]
                           [(? internal:global-bucket?)
                            (translate-global-bucket x)]
                           [(? internal:module-variable?)
                            (translate-module-variable x)]))
                       toplevels)
                  (map translate-stx stxs))]))

(define (translate-stx an-stx)
  (match an-stx
    [(struct internal:stx (encoded))
     (make-stx (translate-wrapped encoded))]))


(define (translate-wrapped a-wrapped)
  (match a-wrapped
    [(struct internal:wrapped (datum wraps certs))
     (make-wrapped datum (map translate-wrap wraps) certs)]))

(define (translate-wrap a-wrap)
  (match a-wrap
    [(? internal:lexical-rename?)
     (translate-lexical-rename a-wrap)]
    [(? internal:phase-shift?)
     (translate-phase-shift a-wrap)]
    [(? internal:module-rename?)
     (translate-module-rename a-wrap)]))

(define (translate-lexical-rename a-lexical-rename)
  (match a-lexical-rename
    [(struct internal:lexical-rename (bool1 bool2 alist))
     (make-lexical-rename bool1 bool2 alist)]))

(define (translate-phase-shift a-phase-shift)
  (match a-phase-shift
    [(struct internal:phase-shift (amt src dest))
     (make-phase-shift amt src dest)]))

(define (translate-module-rename a-module-rename)
  (match a-module-rename
    [(struct internal:module-rename (phase kind set-id unmarshals renames makr-renames plus-kern?))
     (make-module-rename phase kind set-id 
                         (map translate-make-all-from-module unmarshals)
                         (map translate-module-binding renames)
                         mark-renames
                         plus-kern?)]))

(define (translate-make-all-from-module an-all-from-module)
  (match an-all-from-module
    [(struct internal:make-all-from-module (path phase src-phase exceptions prefix))
     (make-make-all-from-module path phase src-phase exceptions prefix)]))
    

(define (translate-module-binding a-module-binding)
  (match a-module-binding
    [(? internal:simple-module-binding?)
     (translate-simple-module-binding a-module-binding)]
    [(? internal:phased-module-binding (path phase export-name nominal-path nominal-export-name))
     (translate-phased-module-binding a-module-binding)]
    [(? internal:exported-nominal-module-binding?)
     (translate-exported-nominal-module-binding a-module-binding)]
    [(? internal:nominal-module-binding?)
     (translate-nominal-module-binding a-module-binding)]
    [(? internal:exported-module-binding?)
     (translate-exported-module-binding a-module-binding)]))

(define (translate-simple-module-binding a-simple-module-binding)
  (match a-simple-module-binding
    [(struct internal:simple-module-binding path)
     (make-simple-module-binding path)]))

(define (translate-phased-module-binding a-binding)
  (match a-binding
    [(struct internal:phased-module-binding (path phase export-name nominal-path nominal-export-name))
     


(define (translate-exported-nominal-module-binding a-module-binding)
  (match a-module-binding
    [(struct internal:exported-nominal-module-binding (path export-name nominal-path nominal-export-name))
     (make-exported-nominal-module-binding path export-name (translate-nominal-path nominal-path) nominal-export-name)]))



(define (translate-module-variable a-module-variable)
  (match a-module-variable
    [(struct internal:module-variable (modidx sym pos phase))
     (make-module-variable modidx sym pos phase)]))

(define (translate-global-bucket a-bucket)
  (match a-bucket
    [(struct internal:global-bucket (name))
     (make-global-bucket name)]))


(define (translate-indirect an-indirect)
  (match an-indirect
    [(struct internal:indirect (v))
     ;; FIXME: avoid cycles: detect if we're already translating this value.
     (make-indirect (translate-closure v))]))



(define (translate-form a-form)
  (match a-form
    [(? internal:def-values?)
     (translate-def-values a-form)]
    [(? internal:def-syntaxes?)
     (translate-def-syntaxes a-form)]
    [(? internal:def-for-syntax?)
     (translate-def-for-syntax a-form)]
    [(? internal:req?)
     (translate-req a-form)]
    [(? internal:seq?)
     (translate-seq a-form)]
    [(? internal:splice?)
     (translate-splice a-form)]
    [(? internal:mod?)
     (translate-mod a-form)]
    [(? internal:expr?)
     (translate-expr a-form)]))


(define (translate-req a-req)
  (match a-req
    [(struct internal:req (reqs dummy))
     (make-req reqs (translate-toplevel dummy))]))
      

(define (translate-toplevel a-toplevel)
  (match a-toplevel
    [(struct internal:toplevel (depth pos const? ready?))
     (make-toplevel depth pos const? ready?)]))


(define (translate-at-expression-position x)
  (match x
    [(? internal:expr?)
     (translate-expr x)]
    [(? internal:seq?)
     (translate-seq x)]
    [(? internal:indirect?)
     (translate-indirect x)]
    [else
     x]))


(define (translate-def-values a-def-values)
  (match a-def-values
    [(struct internal:def-values (ids rhs))
     (make-def-values (map translate-toplevel ids)
                      (translate-at-expression-position rhs))]))


(define (translate-def-syntaxes a-def-syntaxes)
  (match a-def-syntaxes
    [(struct internal:def-syntaxes (ids rhs prefix max-let-depth))
     (make-def-syntaxes (map translate-toplevel ids)
                        (translate-at-expression-position rhs)
                        (translate-prefix prefix)
                        max-let-depth)]))

(define (translate-def-for-syntax a-define-for-syntax)
  (match a-define-for-syntax
    [(struct internal:def-for-syntax (ids rhs prefix max-let-depth))
     (make-def-for-syntax (map translate-toplevel ids)
                          (translate-at-expression-position rhs)
                          (translate-prefix prefix)
                          max-let-depth)]))


(define (translate-mod a-mod)
  (match a-mod
    [(struct internal:mod (name
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
     (make-mod name
               self-modidx 
               (translate-prefix prefix)
               (map (lambda (a-provide) 
                      (list (first a-provide)
                            (translate-provided (second a-provide))
                            (translate-provided (third a-provide))))
                    provides)
               requires
               (map translate-at-form-position body)
               (map (lambda (a-body) 
                      (match a-body 
                        [(? internal:def-syntaxes?)
                         (translate-def-syntaxes a-body)]
                        [(? internal:def-for-syntax?)
                         (translate-def-for-syntax a-body)]))
                    syntax-body)
               unexported
               max-let-depth
               (translate-toplevel dummy)
               lang-info
               internal-context)]))
               
(define (translate-provided a-provided)
  (match a-provided
    [(struct internal:provided (name src src-name nom-mod src-phase protected? insp))
     (make-provided name src src-name nom-mod src-phase protected? insp)]))


(define (translate-splice a-splice)
  (match a-splice
    [(struct internal:splice (forms))
     (make-splice (map translate-at-form-position forms))]))
             
(define (translate-seq a-seq)
  (match a-seq
    [(struct internal:seq (forms))
     (make-seq (map translate-at-form-position forms))]))


(define (translate-at-form-position x)
  (match x
    [(? internal:form?)
     (translate-form x)]
    [(? internal:indirect?)
     (translate-indirect x)]
    [else
     x]))


(define (translate-closure a-closure)
  (match a-closure
    [(struct internal:closure (code gen-id))
     (make-closure (translate-lam code) gen-id)]))


(define (translate-expr an-expr)
  (match an-expr
    [(? internal:lam?)
     (translate-lam an-expr)]
    [(? internal:closure?)
     (translate-closure an-expr)]
    [(? internal:indirect?)
     (translate-indirect an-expr)]
    [(? internal:case-lam?)
     (translate-case-lam an-expr)]
    [(? internal:let-one?)
     (translate-let-one an-expr)]
    [(? internal:let-void?)
     (translate-let-void an-expr)]
    [(? internal:install-value?)
     (translate-install-value an-expr)]
    [(? internal:let-rec?)
     (translate-let-rec an-expr)]
    [(? internal:boxenv?)
     (translate-boxenv an-expr)]
    [(? internal:localref?)
     (translate-localref an-expr)]
    [(? internal:toplevel?)
     (translate-toplevel an-expr)]
    [(? internal:topsyntax?)
     (translate-topsyntax an-expr)]
    [(? internal:application?)
     (translate-application an-expr)]
    [(? internal:branch?)
     (translate-branch an-expr)]
    [(? internal:with-cont-mark?)
     (translate-with-cont-mark an-expr)]
    [(? internal:beg0?)
     (translate-beg0 an-expr)]
    [(? internal:varref?)
     (translate-varref an-expr)]
    [(? internal:assign?)
     (translate-assign an-expr)]
    [(? internal:apply-values?)
     (translate-apply-values an-expr)]
    [(? internal:primval?)
     (translate-primval an-expr)]))


(define (translate-lam a-lam)
  (match a-lam
    [(struct internal:lam 
             (name flags num-params param-types rest? closure-map closure-types max-let-depth body))
     (make-lam name flags num-params param-types rest? closure-map closure-types max-let-depth (translate-at-expression-position body))]))
                      

(define (translate-primval a-primval)
  (match a-primval
    [(struct internal:primval (id))
     (make-primval id)]))

(define (translate-apply-values an-apply-values)
  (match an-apply-values
    [(struct internal:apply-values (proc args-expr))
     (make-apply-values (translate-at-expression-position proc)
                        (translate-at-expression-position args-expr))]))

(define (translate-assign an-assign)
  (match an-assign
    [(struct internal:assign (id rhs undef-ok?))
     (make-assign id (translate-at-expression-position rhs) undef-ok?)]))
             

(define (translate-varref a-varref)
  (match a-varref
    [(struct internal:varref (toplevel))
     (make-varref (translate-toplevel toplevel))]))


(define (translate-beg0 a-beg0)
  (match a-beg0
    [(struct internal:beg0 (seq))
     (make-beg0 (map translate-at-expression-position seq))]))

(define (translate-with-cont-mark a-with-cont-mark)
  (match a-with-cont-mark
    [(struct internal:with-cont-mark (key val body))
     (make-with-cont-mark (translate-at-expression-position key)
                          (translate-at-expression-position val)
                          (translate-at-expression-position body))]))

(define (translate-branch a-branch)
  (match a-branch
    [(struct internal:branch (test then else))
     (make-branch (translate-at-expression-position test)
                  (translate-at-expression-position then)
                  (translate-at-expression-position else))]))


(define (translate-application an-application)
  (match an-application
    [(struct internal:application (rator rands))
     (make-application (translate-at-expression-position rator)
                       (map translate-at-expression-position rands))]))


(define (translate-topsyntax a-topsyntax)
  (match a-topsyntax
    [(struct internal:topsyntax (depth pos midp))
     (make-topsyntax depth pos midp)]))
             

(define (translate-localref a-localref)
  (match a-localref
    [(struct internal:localref (unbox? pos clear? other-clears? flonum?))
     (make-localref unbox? pos clear? other-clears? flonum?)]))

(define (translate-boxenv a-boxenv)
  (match a-boxenv
    [(struct internal:boxenv (pos body))
     (make-boxenv pos (translate-at-expression-position body))]))

(define (translate-let-rec a-let-rec)
  (match a-let-rec
    [(struct internal:let-rec (procs body))
     (make-let-rec (map translate-lam procs)
                   (translate-at-expression-position body))]))


(define (translate-install-value an-install-value)
  (match an-install-value
    [(struct internal:install-value (count pos boxes? rhs body))
     (make-install-value count pos boxes? 
                         (translate-at-expression-position rhs)
                         (translate-at-expression-position body))]))

(define (translate-let-void a-let-void)
  (match a-let-void
    [(struct internal:let-void (count boxes? body))
     (make-let-void (count boxes? (translate-at-expression-position body)))]))


(define (translate-let-one a-let-one)
  (match a-let-one
    [(struct internal:let-one (rhs body flonum?))
     (make-let-one (translate-at-expression-position rhs)
                   (translate-at-expression-position body)
                   flonum?)]))


(define (translate-case-lam a-case-lam)
  (match a-case-lam
    [(struct internal:case-lam (name clauses))
     (make-case-lam name (map translate-lam clauses))]))

