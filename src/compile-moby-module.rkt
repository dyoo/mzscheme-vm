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
         syntax/modcode
         syntax/modresolve)

(define-runtime-path hardcoded-moby-language-path
  "lang/moby-lang.rkt")


(provide/contract [compile-moby-modules
                   (path? . -> . (listof module-record?))])

  
;; Given the path of a scheme program, run it through the compiler
;; and generate the javascript module modules.
;;
;; FIXME: transitively include required modules up to moby-lang.ss.
;; FIXME: use a temporary directory to avoid munging directories with
;; a bunch of zos.
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
                 (jsexp->js (translate-top translated-compilation-top))]
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


;; known-hardcoded-module-path: path -> boolean
(define (known-hardcoded-module-path? p)
  (same-path? p hardcoded-moby-language-path))
  



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


;; get-module-phase-0-requires: compilation-top path? -> (listof path)
(define (get-module-phase-0-requires a-top relative-to)
  (define (resolve mpi)
    (normalize-path 
     (resolve-module-path-index mpi relative-to)))
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
          (map resolve (rest phase0+paths))]))]
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

