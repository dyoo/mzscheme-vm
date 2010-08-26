#lang racket/base

(require "bytecode-translator.rkt"
         "bytecode-structs.rkt"
         "sexp.rkt"
         "non-batch-wrap.rkt"
         "translate-bytecode-structs.rkt"
         "module-record.rkt"
         (prefix-in internal: compiler/zo-parse)
         racket/list
         racket/path
         racket/contract
         syntax/modcode
         syntax/modresolve)


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
    (let loop ([module-record-path #f]
               [a-path a-path])
      (let* ([translated-compilation-top
              (lookup&parse a-path)]
             [translated-program
              (jsexp->js (translate-top translated-compilation-top))])
        (printf "~s\n" (get-module-phase-0-requires translated-compilation-top a-path))
        (cond
          [else
           (list (make-module-record module-record-path translated-program))])))))


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

