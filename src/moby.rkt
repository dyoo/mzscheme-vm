#lang racket/base
(require "bytecode-compiler.rkt"
         "sexp.rkt"
         "non-batch-wrap.rkt"
         "translate-bytecode-structs.rkt"
         "write-runtime.rkt"
         (prefix-in internal: compiler/zo-parse)
         racket/path
         racket/cmdline)

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

  
;; moby: path -> void
;; Given the path of a scheme program, run it through the compiler
;; and generate the javascript program.
(define (moby a-path)
  (let*-values ([(a-path) (normalize-path a-path)]
                [(output-directory) (make-output-file-dir-path a-path)]
                [(base-dir file dir?) (split-path a-path)]
                [(zo-path) (unbatched-compile a-path)]
                [(translated-program)
                 (jsexp->js 
                  (parameterize ([current-directory base-dir]
                                 [current-load-relative-directory base-dir])
                    (compile-top
                     (translate-compilation-top
                      (internal:zo-parse (open-input-file zo-path))))))])

    ;; FIXME: we want to change the flow to
    ;; program -> set of module records
    ;; And then have a separate pass that goes from module records to writing
    ;; out to the file system...
    
    ;; Write out the support runtime.
    (call-with-output-file (build-path output-directory "runtime.js")
      (lambda (op)
        (write-runtime "browser" op))
      #:exists 'replace)

    (copy-support-files output-directory)
    
    ;; Write out the translated program.
    ;; FIXME: write out all the other used modules too.
    (call-with-output-file (build-path output-directory "program.js")
      (lambda (op)
        (fprintf op "var program = (~a);" 
                 translated-program))
      #:exists 'replace)))





(let ([a-path (command-line #:program "mzjs" 
                            #:args (filename)
                            filename)])
  (moby a-path))