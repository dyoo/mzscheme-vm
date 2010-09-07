#lang racket/base

(require "bytecode-translator.rkt"
         "bytecode-structs.rkt"
         "sexp.rkt"
         "translate-bytecode-structs.rkt"
         "module-record.rkt"
         (prefix-in internal: compiler/zo-parse)
         racket/list
         racket/path
         racket/contract
         racket/runtime-path
         racket/match
         syntax/modcode
         syntax/modresolve)

(define-runtime-path hardcoded-moby-kernel-path
  "lang/kernel.rkt")

(define racket-path
  (resolve-module-path 'racket #f))

(define racket/base-path
  (resolve-module-path 'racket/base #f))


(provide/contract [compile-moby-modules
                   (path? . -> . (listof module-record?))])

  
;; Given the path of a scheme program, run it through the compiler
;; and generate the javascript module modules.
(define (compile-moby-modules a-path)
  (let*-values ([(a-path) (normalize-path a-path)])    
    (let loop ([to-visit (list a-path)]
               [acc empty])
      (cond
        [(empty? to-visit)
         acc]
        [else
         (let* ([translated-compilation-top
                 (lookup&parse (first to-visit))]
                [translated-program
                 (jsexp->js (translate-top 
                             (rewrite-module-locations translated-compilation-top
                                                       (first to-visit))))]
                [provides
                 (collect-provided-names translated-compilation-top)]
                [neighbors 
                 (filter-already-visited-modules (get-module-phase-0-requires
                                                  translated-compilation-top (first to-visit))
                                                 (map module-record-path acc))])
           (loop (append neighbors (rest to-visit))
                 (cons (make-module-record (first to-visit) 
                                           translated-program 
                                           provides)
                       acc)))]))))


;; filter-already-visited-modules: (listof path) (listof path) -> (listof path)
(define (filter-already-visited-modules paths visited-paths)
  (filter (lambda (p1)
            (and (not (findf (lambda (p2) (same-path? p1 p2))
                        visited-paths))
                 (not (known-hardcoded-module-path? p1))))
          paths))



  



;; same-path?: path path -> boolean
;; Produces true if both paths are pointing to the same file.
(define (same-path? p1 p2)
  (string=? (path->string (normalize-path p1))
            (path->string (normalize-path p2))))

;; lookup&parse: path -> compilation-top
(define (lookup&parse a-path)
  (let ([op (open-output-bytes)])
    (write (get-module-code a-path) op)
    (translate-compilation-top
     (internal:zo-parse (open-input-bytes (get-output-bytes op))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rewrite-module-locations: compilation-top -> compilation-top
(define (rewrite-module-locations a-top self-path)
  (match a-top
    [(struct compilation-top (max-let-depth prefix code))
     (make-compilation-top max-let-depth 
                           (rewrite-module-locations/prefix prefix self-path)
                           (rewrite-module-locations/code code self-path))]))

(define (rewrite-module-locations/prefix a-prefix self-path)
  (match a-prefix
    [(struct prefix (num-lifts toplevels stxs))
     (make-prefix num-lifts 
                  (map (lambda (t) (rewrite-module-locations/toplevel t self-path))
                       toplevels)
                  stxs)]))

(define (rewrite-module-locations/toplevel a-toplevel self-path)
  (cond
    [(eq? a-toplevel #f)
     a-toplevel]
    [(symbol? a-toplevel)
     a-toplevel]
    [(global-bucket? a-toplevel)
     a-toplevel]
    [(module-variable? a-toplevel)
     (rewrite-module-locations/module-variable a-toplevel self-path)]))
  
          
(define (rewrite-module-locations/module-variable a-module-variable self-path)
  (match a-module-variable
    [(struct module-variable (modidx sym pos phase))
     (make-module-variable (rewrite-module-locations/modidx modidx self-path) sym pos phase)]))


;; known-hardcoded-module-path: path -> boolean
(define (known-hardcoded-module-path? p)
  (let ([hardcoded-modules
         (list hardcoded-moby-kernel-path
               #;racket-path
               #;racket/base-path)])
    (ormap (lambda (h)
             (same-path? p h))
           hardcoded-modules)))


;; rewrite-to-hardcoded-module-path: module-path-index path -> module-path-index
(define (rewrite-module-locations/modidx a-modidx self-path)
  (let ([resolved-path (resolve-module-path-index a-modidx self-path)])
    (cond
      [(symbol? resolved-path)
       a-modidx]
      [(same-path? resolved-path hardcoded-moby-kernel-path)
       ;; rewrite to a (possibly fictional) collection named moby/moby-lang
       ;; The runtime will recognize this collection.
       (module-path-index-join 'moby/kernel
                               (module-path-index-join #f #f))]
      [(same-path? resolved-path racket-path)
       a-modidx]
      [(same-path? resolved-path racket/base-path)
       a-modidx]
      [else
       a-modidx])))


(define (rewrite-module-locations/code a-code self-path)
  (match a-code
    [(struct mod (name self-modidx prefix provides requires body syntax-body unexported max-let-depth dummy lang-info internal-context))
     (make-mod name 
               (rewrite-module-locations/modidx self-modidx self-path)
               (rewrite-module-locations/prefix prefix self-path)
               (map (lambda (phase+provided) 
                      (list (first phase+provided)
                            (map (lambda (p) (rewrite-module-locations/provided p self-path))
                                 (second phase+provided))
                            (map (lambda (p) (rewrite-module-locations/provided p self-path))
                                 (third phase+provided))))
                    provides)
               (map (lambda (phase+requires) 
                      (cons (first phase+requires)
                            (map (lambda (p) (rewrite-module-locations/modidx p self-path))
                                 (rest phase+requires))))
                    requires)
               body 
               syntax-body 
               unexported 
               max-let-depth
               dummy 
               lang-info 
               internal-context)]
    [else
     a-code]))

(define (rewrite-module-locations/provided a-provided self-path)
  (match a-provided
    [(struct provided (name src src-name nom-src src-phase protected? insp))
     (make-provided name 
                    (if src (rewrite-module-locations/modidx src self-path) src)
                    src-name
                    nom-src
                    src-phase 
                    protected? 
                    insp)]))
                                          



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; get-module-phase-0-requires: compilation-top path? -> (listof path)
(define (get-module-phase-0-requires a-top relative-to)
  ;; resolve: module-path-index -> (path | '#%kernel) 
  (define (resolve mpi)
    (resolve-module-path-index mpi relative-to))
  (cond
    [(mod? (compilation-top-code a-top))
     (let* ([a-mod (compilation-top-code a-top)]
            [requires (mod-requires a-mod)]
            [phase0+paths
             (findf (lambda (phase+paths)
                      (= (first phase+paths) 0))
                    requires)])
       (cond
         [(eq? phase0+paths #f)
          empty]
         [else
          (map normalize-path
               (filter path? (map resolve (rest phase0+paths))))]))]
    [else
     empty]))


;; collect-provided-names: compilation-top -> (listof symbol)
(define (collect-provided-names a-top)
  (define (get-name a-provided)
    (provided-name a-provided))
  (cond
    [(mod? (compilation-top-code a-top))
     (let* ([a-mod (compilation-top-code a-top)]
            [provides (mod-provides a-mod)]
            [phase0+provides
             (findf (lambda (phase+provides)
                      (= (first phase+provides) 0))
                    provides)])
       (cond
         [(eq? phase0+provides #f)
          empty]
         [else
          ;; FIXME: currently ignoring exported syntax.
          (map get-name (second phase0+provides))]))]
    [else
     empty]))




;; make-output-file-path: path -> path
;; Given the normalized name of the Scheme program, produce a normalized path
;; of the output javascript application.
(define (make-output-file-dir-path a-file-path)
  (let*-values ([(base file dir?)
                 (split-path a-file-path)]
                [(new-directory-path) 
                 (normalize-path
                  (build-path base 
                              (remove-filename-extension
                               (file-name-from-path file))))])
    (unless (directory-exists? new-directory-path)
      (make-directory new-directory-path))
    new-directory-path))


;; remove-filename-extension: path-string -> path-string
;; Removes the filename extension portion.
(define (remove-filename-extension a-path)
  (let ([p (if (path? a-path)
               (path->string a-path)
               a-path)])
    (regexp-replace #px"\\.\\w+$" p "")))

