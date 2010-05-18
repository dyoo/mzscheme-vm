#lang scheme/base
(require "bytecode-compiler.ss"
         "sexp.ss"
         "batch-wrap.ss"
         compiler/zo-parse
         #;compiler/compiler
         scheme/path
         scheme/cmdline)

(define (make-output-path a-path)
  (regexp-replace #px"\\.\\w+$" 
                  (path->string (file-name-from-path a-path))
                  ".js"))

;; mzjs: path -> void
;; Given the path of a scheme program, run it through the batch compiler
;; and generate the javascript program.
(define (mzjs a-path)
  ;; Run the batch compiler over the path
  (let ([compiled-zo-path (batch-compile a-path)])
    (call-with-input-file compiled-zo-path
      (lambda (ip)
        (call-with-output-file (make-output-path a-path)
          (lambda (op)
            (fprintf op "var _runtime = require('./../../lib');")
            (fprintf op "var program = _runtime.load(~a);" 
                     (jsexp->js (compile-top (zo-parse ip))))
            (fprintf op "_runtime.run(program);"))
          #:exists 'replace)))))


#;(mzjs (command-line #:program "mzjs" 
              #:args (filename)
              filename))
