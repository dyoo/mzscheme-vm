#lang racket/base
(require "private/create-javascript-package.rkt"
         "private/misc.rkt"
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
                  (build-path (cond 
                                [(path? base )
                                 base]
                                [(eq? base 'relative)
                                 (current-directory)]
                                [(eq? base #f)
                                 "/"])
                              (remove-filename-extension
                               (file-name-from-path file))))])
    (unless (directory-exists? new-directory-path)
      (make-directory new-directory-path))
    new-directory-path))



#;(create-javascript-package "../tests/moby-programs/require.rkt")
#;(create-javascript-package (build-path "../tests/mz-tests/basic.rktl")
                           (build-path "test-output"))

(let ([a-path (command-line #:program "mzjs" 
                            #:args (filename)
                            filename)])
  (create-javascript-package (build-path a-path)
                             (make-output-file-dir-path a-path)))