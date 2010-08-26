#lang racket/base

(require "bytecode-translator.rkt"
         "sexp.rkt"
         "non-batch-wrap.rkt"
         "translate-bytecode-structs.rkt"
         "module-record.rkt"
         (prefix-in internal: compiler/zo-parse)
         racket/path
         racket/contract)


(provide/contract [compile-moby-modules
                   (path? . -> . (listof module-record?))])

  
;; Given the path of a scheme program, run it through the compiler
;; and generate the javascript module modules.
;;
;; FIXME: transitively include required modules up to moby-lang.ss.
;; FIXME: use a temporary directory to avoid munging directories with
;; a bunch of zos.
(define (compile-moby-modules a-path)
  (let*-values ([(a-path) (normalize-path a-path)]
                [(output-directory) 
                 (make-output-file-dir-path a-path)]
                [(base-dir file dir?) 
                 (split-path a-path)]
                [(zo-path) 
                 (unbatched-compile a-path)]
                [(translated-compilation-top 
                  (parameterize ([current-directory base-dir]
                                 [current-load-relative-directory base-dir])
                    (translate-compilation-top
                     (internal:zo-parse (open-input-file zo-path)))))]
                [(translated-program)
                 (jsexp->js (translate-top translated-compilation-top))])
    (list (make-module-record #f translated-program))))



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

