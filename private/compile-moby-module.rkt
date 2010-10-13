#lang racket/base

(require "bytecode-translator.rkt"
         "bytecode-structs.rkt"
         "sexp.rkt"
         "translate-bytecode-structs.rkt"
         "module-record.rkt"
         "collect-unimplemented-primvals.rkt"
         (prefix-in permissions: "../permissions/query.rkt")
         (prefix-in js-impl: "../lang/js-impl/query.rkt")
         (prefix-in internal: compiler/zo-parse)
         racket/list
         racket/path
         racket/contract
         racket/runtime-path
         racket/match
         syntax/modcode
         syntax/modresolve)

(define-runtime-path mzscheme-vm-src-directory "..")

(define-runtime-path hardcoded-moby-kernel-path
  "../lang/kernel.rkt")

(define-runtime-path hardcoded-moby-paramz-path
  "../lang/paramz.rkt")

(define-runtime-path hardcoded-js-impl-path
  "../lang/js-impl/js-impl.rkt")

(define racket-path
  (resolve-module-path 'racket #f))

(define racket/base-path
  (resolve-module-path 'racket/base #f))

(define racket-private-modbeg-path
  (resolve-module-path 'racket/private/modbeg #f))


(provide/contract [compile-moby-modules
                   (path? . -> . (listof module-record?))])


;; compile-module-modules: path -> (listof module-record)
;; Given the path of a scheme program, run it through the compiler
;; and generate the javascript module modules.
(define (compile-moby-modules main-module-path)
  (let*-values ([(a-path) (normalize-path main-module-path)])    
    (let loop ([to-visit (list a-path)]
               [module-records empty])
      (cond
        [(empty? to-visit)
         module-records]
        [else
         (let* ([record (compile-moby-module (first to-visit) (normalize-path main-module-path))]
                [neighbors (filter-already-visited-modules
                            (module-neighbors (first to-visit))
                            (map module-record-path module-records))])
           (loop (append neighbors (rest to-visit))
                 (cons record module-records)))]))))



;; module-neighbors: path -> (listof path)
;; Returns a list of the required modules needed by the module of the given a-path.
(define (module-neighbors a-path)
  (let* ([translated-compilation-top
             (lookup&parse a-path)]
            [neighbors 
             (get-module-phase-0-requires
              translated-compilation-top a-path)])
       neighbors))
                           

;; compile-moby-module: path path -> module-record
(define (compile-moby-module a-path main-module-path)
  (cond
    [(looks-like-js-implemented-module? a-path)
     =>
     (lambda (a-js-impl-record)
       (make-js-module-record (munge-resolved-module-path-to-symbol a-path main-module-path)
                              a-path
                              (apply string-append (js-impl:js-module-impls a-js-impl-record))
                              (js-impl:js-module-exports a-js-impl-record)
                              (map (lambda (a-path) 
                                     (munge-resolved-module-path-to-symbol a-path main-module-path)) 
                                   (filter (negate known-hardcoded-module-path?) 
                                           (module-neighbors a-path)))
                              '()
                              '()))]
    [else
     (let* ([translated-compilation-top
             (lookup&parse a-path)]
            [translated-jsexp
             (translate-top 
              (rewrite-module-locations/compilation-top translated-compilation-top
                                                        a-path
                                                        main-module-path))]
            [translated-program
             (jsexp->js translated-jsexp)]
            [unimplemented-primvals
             (collect-unimplemented-primvals translated-jsexp)]
            [permissions
             (permissions:query `(file ,(path->string a-path)))]
            [provides
             (collect-provided-names translated-compilation-top)])
       (make-module-record (munge-resolved-module-path-to-symbol a-path main-module-path)
                           a-path
                           translated-program 
                           provides
                           (map (lambda (a-path) 
                                  (munge-resolved-module-path-to-symbol a-path main-module-path)) 
                                (filter (negate known-hardcoded-module-path?)
                                        (module-neighbors a-path)))
                           permissions
                           unimplemented-primvals))]))

;; negate: (X -> boolean) -> (X -> boolean)
;; Negates a predicate.
(define (negate pred)
  (lambda (x)
    (not (pred x))))


;; looks-like-js-implemented-module?: path -> (or false
(define (looks-like-js-implemented-module? a-path)
  (js-impl:query `(file ,(path->string a-path))))


;; filter-already-visited-modules: (listof path) (listof path) -> (listof path)
(define (filter-already-visited-modules paths visited-paths)
  (filter (lambda (p1)
            (and (not (findf (lambda (p2) (same-path? p1 p2))
                        visited-paths))
                 (not (known-hardcoded-module-path? p1))))
          paths))




;; known-hardcoded-module-path: path -> boolean
;; Returns true if the module should be considered hardcoded,
;; where module visiting should consider the given module as a
;; leaf.
(define (known-hardcoded-module-path? p)
  (let ([hardcoded-modules
         (list hardcoded-moby-kernel-path
               hardcoded-moby-paramz-path
               hardcoded-js-impl-path)])
    (ormap (lambda (h)
             (same-path? p h))
           hardcoded-modules)))


  



;; same-path?: path path -> boolean
;; Produces true if both paths are pointing to the same file.
(define (same-path? p1 p2)
  (string=? (path->string (normalize-path p1))
            (path->string (normalize-path p2))))

(define ns (make-base-empty-namespace))

;; lookup&parse: path -> compilation-top
(define (lookup&parse a-path)
  (let ([op (open-output-bytes)])
    (write (parameterize ([current-namespace ns])
             (get-module-code a-path))
           op)
    (translate-compilation-top
     (internal:zo-parse (open-input-bytes (get-output-bytes op))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; munge-resolve-module-path-to-symbol path path -> symbol 
;; We rewrite module indexes all to symbolic path references.
(define (munge-resolved-module-path-to-symbol a-resolved-module-path main-module-path)
  (let-values ([(base file dir?) (split-path (normalize-path main-module-path))])
    (cond
      [(symbol? a-resolved-module-path)
       a-resolved-module-path]
      [else
       (cond 
         ;; If a subdirectory to the mzscheme-vm path, 
         ;; rename relative to it.
         [(subdirectory-of? (let-values ([(d name dir?)
                                          (split-path a-resolved-module-path)])
                              d)
                            mzscheme-vm-src-directory)
          (let ([relative (find-relative-path (normalize-path mzscheme-vm-src-directory)
                                              (normalize-path a-resolved-module-path))])
            (string->symbol (string-append "mzscheme-vm/"
                                           (remove-extension (path->string relative)))))]
         [else
          (let ([relative (find-relative-path base
                                              (normalize-path a-resolved-module-path))])
            (string->symbol (string-append "relative/"
                                           (replace-dots
                                            (replace-up-dirs 
                                             (remove-extension 
                                              (path->string relative)))))))])])))


(define filesystem-roots (filesystem-root-list))

;; subdirectory-of?: directory-path directory-path -> boolean
;; Returns true if a-file-path lives within base-dir somewhere.
(define (subdirectory-of? a-dir parent-dir)
  (let loop ([a-dir a-dir])
    (cond
      [(same-path? a-dir parent-dir)
       #t]
      [(member a-dir filesystem-roots)
       #f]
      [else
       (loop (normalize-path (build-path a-dir 'up)))])))



;; replace-up-dirs: string -> string
(define (replace-up-dirs a-str)
  (regexp-replace* #px"\\.\\./" a-str "up/"))

;; replace-dots: string -> string
(define (replace-dots a-str)
  (regexp-replace* #px"\\." a-str "-dot-"))

;; remove-extension: string -> string
(define (remove-extension a-str)
  (regexp-replace* #px"\\.[^\\.]+$" a-str ""))





;; rewrite-module-locations: compilation-top -> compilation-top
(define (rewrite-module-locations/compilation-top a-top self-path main-module-path)
  (match a-top
    [(struct compilation-top (max-let-depth prefix code))
     (make-compilation-top max-let-depth 
                           (rewrite-module-locations/prefix prefix self-path main-module-path)
                           (rewrite-module-locations/code code self-path main-module-path))]))

(define (rewrite-module-locations/prefix a-prefix self-path main-module-path)
  (match a-prefix
    [(struct prefix (num-lifts toplevels stxs))
     (make-prefix num-lifts 
                  (map (lambda (t) (rewrite-module-locations/toplevel t self-path main-module-path))
                       toplevels)
                  stxs)]))

(define (rewrite-module-locations/toplevel a-toplevel self-path main-module-path)
  (cond
    [(eq? a-toplevel #f)
     a-toplevel]
    [(symbol? a-toplevel)
     a-toplevel]
    [(global-bucket? a-toplevel)
     a-toplevel]
    [(module-variable? a-toplevel)
     (rewrite-module-locations/module-variable a-toplevel self-path main-module-path)]))
  
          
(define (rewrite-module-locations/module-variable a-module-variable self-path main-module-path)
  (match a-module-variable
    [(struct module-variable (modidx sym pos phase))
     (make-module-variable (rewrite-module-locations/modidx modidx self-path main-module-path)
                           sym pos phase)]))



;; rewrite-module-locations/modidx: module-path-index path -> module-path-index
(define (rewrite-module-locations/modidx a-modidx self-path main-module-path)
  (let ([resolved-path (resolve-module-path-index a-modidx self-path)])
    (cond
      [(symbol? resolved-path)
       a-modidx]
      
      [(same-path? resolved-path hardcoded-moby-kernel-path)
       ;; rewrite to a (possibly fictional) collection named moby/moby-lang
       ;; The runtime will recognize this collection.
       (module-path-index-join 'moby/kernel
                               (module-path-index-join #f #f))]
      [(same-path? resolved-path hardcoded-moby-paramz-path)
       ;; rewrite to a (possibly fictional) collection named moby/paramz
       ;; The runtime will recognize this collection.
       (module-path-index-join 'moby/paramz
                               (module-path-index-join #f #f))]

      [(same-path? resolved-path hardcoded-js-impl-path)
       ;; rewrite to a (possibly fictional) collection named moby/js-impl
       ;; The runtime will recognize this collection.
       (module-path-index-join 'moby/js-impl
                               (module-path-index-join #f #f))]
      
      ;; KLUDGE!!! We should NOT be reusing the private implementation of module
      ;; begin.  I have to fix this as soon as I have time and priority.
      [(same-path? resolved-path racket-private-modbeg-path)
       (module-path-index-join 'moby/kernel
                               (module-path-index-join #f #f))]
                   
      [else
       (let* ([renamed-path-symbol 
               (munge-resolved-module-path-to-symbol resolved-path main-module-path)])
         (module-path-index-join renamed-path-symbol
                                 (module-path-index-join #f #f)))])))




(define (rewrite-module-locations/code a-code self-path main-module-path)
  (match a-code
    [(struct mod (name self-modidx prefix provides requires body syntax-body unexported max-let-depth dummy lang-info internal-context))
     (make-mod name 
               (rewrite-module-locations/modidx self-modidx self-path main-module-path)
               (rewrite-module-locations/prefix prefix self-path main-module-path)
               (map (lambda (phase+provided) 
                      (list (first phase+provided)
                            (map (lambda (p) (rewrite-module-locations/provided p self-path main-module-path))
                                 (second phase+provided))
                            (map (lambda (p) (rewrite-module-locations/provided p self-path main-module-path))
                                 (third phase+provided))))
                    provides)
               (map (lambda (phase+requires) 
                      (cons (first phase+requires)
                            (map (lambda (p) (rewrite-module-locations/modidx p self-path main-module-path))
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

(define (rewrite-module-locations/provided a-provided self-path main-module-path)
  (match a-provided
    [(struct provided (name src src-name nom-src src-phase protected? insp))
     (make-provided name 
                    (if src (rewrite-module-locations/modidx src self-path main-module-path) src)
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

