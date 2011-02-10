#lang s-exp "profiled-base.rkt"

(require "bytecode-translator.rkt"
         "bytecode-structs.rkt"
         "sexp.rkt"
         "translate-bytecode-structs.rkt"
         "module-record.rkt"
         "collect-unimplemented-primvals.rkt"
         "path-helpers.rkt"
         "get-interaction-bytecode.rkt"
         (prefix-in permissions: "../permissions/query.rkt")
         (prefix-in js-impl: "../lang/js-impl/query.rkt")
         (prefix-in js-conditional: "../lang/js-conditional/query.rkt")
         (prefix-in internal: compiler/zo-parse)
         racket/list
         racket/path
         racket/contract
         racket/runtime-path
         racket/match
	 racket/string
         syntax/modcode
         syntax/modresolve)

(define-runtime-path mzscheme-vm-src-directory "..")

(define-runtime-path hardcoded-moby-kernel-path
  "../lang/kernel.rkt")

(define-runtime-path hardcoded-moby-paramz-path
  "../lang/paramz.rkt")

(define-runtime-path hardcoded-js-impl-path
  "../lang/js-impl/js-impl.rkt")

(define-runtime-path hardcoded-js-conditional-path
  "../lang/js-conditional/js-conditional.rkt")


(define racket-path
  (resolve-module-path 'racket #f))

(define racket/base-path
  (resolve-module-path 'racket/base #f))

(define racket-private-modbeg-path
  (resolve-module-path 'racket/private/modbeg #f))


(define compilation-top-cache (make-parameter (make-hash)))


(define (lazy-stream? x)
  (or (empty? x)
      (and (cons? x)
           (procedure? (cdr x)))))


(provide/contract [compile-moby-modules
                   (path? . -> . (listof module-record?))]
                  
                  [compile-moby-modules/lazy
                   ((listof path?) path? . -> . lazy-stream?)]
                  
                  [compile-module
                   (path? path? . -> . module-record?)]

                  [compile-plain-racket-module
                   ((or/c path? false/c)
                    (or/c path? false/c) 
                    input-port?
                    . -> . 
                    module-record?)]
                  
                  [compile-interaction
                   (module-path? any/c . -> . interaction-record?)]
                  
                  
                  [get-module-bytecode/port
                   (any/c input-port? . -> . input-port?)])


;; compile-module-modules: path -> (listof module-record)
;; Given the path of a scheme program, run it through the compiler
;; and generate the javascript module modules.
(define (compile-moby-modules main-module-path)
  (parameterize ([compilation-top-cache (make-hash)])
    (let ([a-path (normalize-path main-module-path)])
      (let loop ([to-visit (list a-path)]
                 [module-records empty]
                 [visited-paths empty])
        (cond
          [(empty? to-visit)
           module-records]
          [(ormap (lambda (p)
                    (same-module-record-path? p (first to-visit)))
                  visited-paths)
           (loop (rest to-visit)
                 module-records
                 visited-paths)]
          [else
           (let* ([record (compile-module 
                           (first to-visit) 
                           a-path)]
                  [neighbors (filter-already-visited-modules+hardcodeds
                              (module-neighbors (first to-visit))
                              visited-paths)])
             (loop (append neighbors (rest to-visit))
                   (cons record module-records)
                   (cons (module-record-path record) visited-paths)))])))))


;; compile-module-modules/lazy: (listof path) path -> (lazy-streamof module-record)
(define (compile-moby-modules/lazy module-paths main-module-path)
  (parameterize ([compilation-top-cache (make-hash)])
    (let ([main-module-path (normalize-path main-module-path)]
          [module-paths (map normalize-path module-paths)])
      (let loop ([to-visit module-paths]
                 [visited-paths empty])
        (cond
          [(empty? to-visit)
           empty]
          [(ormap (lambda (p)
                    (same-module-record-path? p (first to-visit)))
                  visited-paths)
           (loop (rest to-visit)
                 visited-paths)]
          [else
           (let* ([record (compile-module (first to-visit) main-module-path)]
                  [neighbors (filter-already-visited-modules+hardcodeds
                              (module-neighbors (first to-visit))
                              visited-paths)])
             (cons record
                   (lambda ()
                     (loop (append neighbors (rest to-visit))
                           (cons (module-record-path record) visited-paths)))))])))))







;; module-neighbors: path -> (listof path)
;; Returns a list of the required modules needed by the module of the given a-path.
(define (module-neighbors a-path)
  (cond [(looks-like-js-conditional-module? a-path)
         '()]
        [else
         (let* ([translated-compilation-top
                 (parse-and-translate (get-module-bytecode/path a-path))]
                [neighbors 
                 (get-module-phase-0-requires
                  translated-compilation-top a-path)])
           neighbors)]))


;; compile-module: path path -> module-record
(define (compile-module a-path main-module-path)
  (cond
    [(looks-like-js-implemented-module? a-path)
     =>
     (lambda (a-js-impl-record)
       (compile-js-implementation a-path main-module-path a-js-impl-record))]
    [(looks-like-js-conditional-module? a-path)
     (compile-js-conditional-module a-path main-module-path)]
    [else
     (compile-plain-racket-module a-path main-module-path 
                                  (get-module-bytecode/path a-path))]))

;; compile-js-implementation: path path -> module-record
(define (compile-js-implementation a-path main-module-path a-js-impl-record)
  (make-js-module-record 
   (munge-resolved-module-path-to-symbol a-path main-module-path)
   a-path
   (string-join (js-impl:js-module-impls a-js-impl-record) "")
   (js-impl:js-module-exports a-js-impl-record)
   (map (lambda (a-path) 
          (munge-resolved-module-path-to-symbol a-path main-module-path)) 
        (filter (negate known-hardcoded-module-path?) 
                (module-neighbors a-path)))
   '()
   '()))

;; compile-js-conditional-module: path path -> module-record
(define (compile-js-conditional-module a-path main-module-path)
  (let* ([translated-compilation-top (parse-and-translate
                                      (get-module-bytecode/path a-path))]
         [exports (collect-provided-names translated-compilation-top)])
    (make-js-module-record (munge-resolved-module-path-to-symbol a-path main-module-path)
                           a-path
                           (js-conditional:query `(file ,(path->string a-path)))
                           exports
                           (list)
                           (list)
                           (list))))


;; compile-plain-racket-module: (or path #f) (or path #f) input-port -> module-record
(define (compile-plain-racket-module a-path
                                     main-module-path
                                     bytecode-ip)
  (let* ([translated-compilation-top (parse-and-translate bytecode-ip)]
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
          (if a-path 
              (permissions:query `(file ,(path->string a-path)))
              '())]
         [provides
          (collect-provided-names translated-compilation-top)]
         
         [name (if a-path
                   (munge-resolved-module-path-to-symbol 
                    a-path 
                    main-module-path)
                   #f)]
         [requires
          (map (lambda (a-path) 
                 (munge-resolved-module-path-to-symbol a-path main-module-path)) 
               (filter (negate known-hardcoded-module-path?)
                       (get-module-phase-0-requires translated-compilation-top a-path)))])
    (make-module-record name
                        a-path
                        translated-program 
                        provides
                        requires
                        permissions
                        unimplemented-primvals)))


;; compile-interaction: path input-port -> interaction-record
(define (compile-interaction lang stx)
  (let* ([bytecode-bytes (get-interaction-bytecode stx
                                                   #:language-module lang)]
         [translated-compilation-top
          (translate-compilation-top 
           (internal:zo-parse 
            (open-input-bytes bytecode-bytes)))]
         [translated-jsexp
          (translate-top
           (rewrite-module-locations/compilation-top 
            translated-compilation-top
            #f
            #f))]
         [translated-program
          (jsexp->js translated-jsexp)])
    (make-interaction-record translated-program)))




;; negate: (X -> boolean) -> (X -> boolean)
;; Negates a predicate.
(define (negate pred)
  (lambda (x)
    (not (pred x))))


;; looks-like-js-implemented-module?: path -> (or false list)
(define (looks-like-js-implemented-module? a-path)
  (js-impl:query `(file ,(path->string a-path))))


;; looks-like-js-conditional-module?: path -> boolean
(define (looks-like-js-conditional-module? a-path)
  (js-conditional:has-javascript-implementation? `(file ,(path->string a-path))))


;; filter-already-visited-modules: (listof path) (listof path) -> (listof path)
(define (filter-already-visited-modules+hardcodeds paths visited-paths)
  (let ([result
         (filter (lambda (p1)
                   (cond
                     [(findf (lambda (p2) (same-module-record-path? p1 p2)) visited-paths)
                      #f]
                     [(known-hardcoded-module-path? p1)
                      #f]
                     [else
                      #t]))
                 paths)])
    result))




;; known-hardcoded-module-path: path -> boolean
;; Returns true if the module should be considered hardcoded,
;; where module visiting should consider the given module as a
;; leaf.
(define (known-hardcoded-module-path? p)
  (let ([hardcoded-modules
         (list hardcoded-moby-kernel-path
               hardcoded-moby-paramz-path
               hardcoded-js-impl-path
               hardcoded-js-conditional-path)])
    (ormap (lambda (h)
             (same-module-record-path? p h))
           hardcoded-modules)))






;; same-path?: path path -> boolean
;; Produces true if both paths are pointing to the same file.
(define (same-module-record-path? p1 p2)
  (= (file-or-directory-identity p1)
     (file-or-directory-identity p2)))




(define ns (make-base-empty-namespace))

;; memoize/parameter: (parameterof (hashof X Y)) (X -> Y) ->  (X -> Y)
;; Produces a memoized wrapping of f that checks to see if
;; we've seen this input already.
(define (memoize/parameter ht-param f)
  (lambda (x)
    (hash-ref (ht-param) x 
              (lambda ()
                (let ([result
                       (f x)])
                  (hash-set! (ht-param) x result)
                  result)))))



;; get-module-bytecode/path: path -> input-port
;; Returns an input port with the bytecode of the module.
(define get-module-bytecode/path
  (let ([code-lookup
         (memoize/parameter
          compilation-top-cache
          (lambda (a-path)
            (parameterize ([current-namespace ns])
              (get-module-code a-path))))])
    (lambda (a-path)
      (let ([op (open-output-bytes)])
        (write (code-lookup a-path) op)
        (open-input-bytes (get-output-bytes op))))))


;; get-module-bytecode/port: any input-port -> input-port
;; Returns an input port with the bytecode of the module.
(define (get-module-bytecode/port name ip)
  (parameterize ([read-accept-reader #t]
                 [current-namespace ns])
    (namespace-require 'racket/base)
    (let ([stx (read-syntax name ip)]
          [op (open-output-bytes)])
      (write (compile stx) op)
      (open-input-bytes (get-output-bytes op)))))



;; parse-and-translate: input-port -> compilation-top
(define (parse-and-translate ip)
  (translate-compilation-top
   (internal:zo-parse ip)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; munge-resolve-module-path-to-symbol path (or path #f) -> symbol 
;; We rewrite module indexes all to symbolic path references.
(define (munge-resolved-module-path-to-symbol a-resolved-module-path main-module-path)
  (let-values ([(base file dir?) (if main-module-path
                                     (split-path
                                      (normalize-path main-module-path))
                                     (values (or (current-load-relative-directory)
                                                 (current-directory))
                                             #f
                                             #f))])
    (cond
      [(symbol? a-resolved-module-path)
       a-resolved-module-path]
      
      [(path? a-resolved-module-path)
       (let ([normalized-resolved-module-path
              (normalize-path a-resolved-module-path)])
         (cond
           [(js-conditional:redirected? normalized-resolved-module-path)
            (munge-resolved-module-path-to-symbol 
             (js-conditional:follow-redirection normalized-resolved-module-path)
             main-module-path)]
           
           ;; If a subdirectory to the mzscheme-vm path, 
           ;; rename relative to it.
           [(subdirectory-of? (let-values ([(d name dir?)
                                            (split-path normalized-resolved-module-path)])
                                d)
                              mzscheme-vm-src-directory)
            (let ([relative (find-relative-path (normalize-path mzscheme-vm-src-directory)
                                                normalized-resolved-module-path)])
              (string->symbol 
               (string-append "mzscheme-vm/"
                              (munge-path-string (path->string relative)))))]
           [else
            (let ([relative (find-relative-path base
                                                normalized-resolved-module-path)])
              (string->symbol (string-append "relative/"
                                             (munge-path-string (path->string relative)))))]))]
      [else
       (error 'munge-resolved-module-path-to-symbol a-resolved-module-path)])))

;; munge-path-string-to-symbol: string -> string
(define (munge-path-string a-str)
  (replace-other-forbidden-chars
   (replace-dots
    (replace-up-dirs 
     (replace-backslashes-with-forwards
      (remove-extension a-str))))))


(define (replace-backslashes-with-forwards a-str)
  (regexp-replace* #px"[\\\\]" a-str "/"))




#|
According to the documentation, module paths must fit the following description:


A path relative to the containing source (as determined by
current-load-relative-directory or current-directory). Regardless
of the current platform, rel-string is always parsed as a Unix-format
relative path: / is the path delimiter (multiple adjacent /s are treated as a
single delimiter), .. accesses the parent directory, and . accesses the current
directory. The path cannot be empty or contain a leading or trailing slash, path
elements before than the last one cannot include a file suffix (i.e., a . in an element
other than . or ..), and the only allowed characters are ASCII letters, ASCII digits, -,
+, _, ., /, and %. Furthermore, a % is allowed only when followed by two lowercase
hexadecimal digits, and the digits must form a number that is not the ASCII value of a
letter, digit, -, +, or _.

|#
(define replace-other-forbidden-chars
  (let ([n 0]
        [ht (make-hash)]
        [forbidden-regexp #px"[^a-zA-Z0-9\\-\\+\\_\\.\\/]"])
    (lambda (a-str)
      (hash-ref ht a-str
                (lambda ()
                  (cond
                    [(regexp-match forbidden-regexp a-str)
                     =>
                     (lambda (a-match)
                       (set! n (add1 n))
                       (hash-set! ht a-str
                                  (string-append 
                                   (regexp-replace* forbidden-regexp a-str "")
                                   (number->string n)))
                       (hash-ref ht a-str))]
                    [else
                     a-str]))))))


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
    [(struct compilation-top (prefix code))
     (make-compilation-top (rewrite-module-locations/prefix prefix self-path main-module-path)
                           (rewrite-module-locations/code code self-path main-module-path))]))

(define (rewrite-module-locations/prefix a-prefix self-path main-module-path)
  (match a-prefix
    [(struct prefix (toplevels stxs))
     (make-prefix (map (lambda (t) (rewrite-module-locations/toplevel t self-path main-module-path))
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
  (let ([resolved-path (maybe-correct-rkt-to-ss 
                        (resolve-module-path-index a-modidx self-path))])
    (cond
      [(symbol? resolved-path)
       a-modidx]
      
      [(same-module-record-path? resolved-path hardcoded-moby-kernel-path)
       ;; rewrite to a (possibly fictional) collection named moby/moby-lang
       ;; The runtime will recognize this collection.
       (module-path-index-join 'moby/kernel
                               (module-path-index-join #f #f))]
      [(same-module-record-path? resolved-path hardcoded-moby-paramz-path)
       ;; rewrite to a (possibly fictional) collection named moby/paramz
       ;; The runtime will recognize this collection.
       (module-path-index-join 'moby/paramz
                               (module-path-index-join #f #f))]
      
      [(same-module-record-path? resolved-path hardcoded-js-impl-path)
       ;; rewrite to a (possibly fictional) collection named moby/js-impl
       ;; The runtime will recognize this collection.
       (module-path-index-join 'moby/js-impl
                               (module-path-index-join #f #f))]
      [(same-module-record-path? resolved-path hardcoded-js-conditional-path)
       (module-path-index-join 'moby/js-conditional
                               (module-path-index-join #f #f))]
      
      ;; KLUDGE!!! We should NOT be reusing the private implementation of module
      ;; begin.  I have to fix this as soon as I have time and priority.
      [(same-module-record-path? resolved-path racket-private-modbeg-path)
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
          (map (lambda (p) (maybe-correct-rkt-to-ss (normalize-path p)))
               (filter path? (map resolve (rest phase0+paths))))]))]
    [else
     empty]))


;; maybe-correct-rkt-to-ss: resolved-module-path -> resolved-module-path
;; Given a module path, possibly repair the damage that the module resolver
;; is doing in giving us a normalized .rkt path, even if the module in question
;; is a .ss file.
(define (maybe-correct-rkt-to-ss p)
  (cond [(path? p) 
         (cond
           [(file-exists? p)
            p]
           [else
            (let* ([ps (path->string p)]
                   [ss-path (build-path 
                             (string-append
                              (substring ps 0 
                                         (- (string-length ps)
                                            (bytes-length 
                                             (filename-extension p))))
                              "ss"))])
              (cond [(file-exists? ss-path)
                     ss-path]
                    [else
                     p]))])]
        [else
         p]))



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

