#lang scheme/base
(require "bytecode-compiler.ss"
         "sexp.ss"
         compiler/zo-parse
         scheme/path
         scheme/cmdline)

(define (make-output-path a-path)
  (regexp-replace #px"\\.\\w+$" 
                  (path->string (file-name-from-path a-path))
                  ".js"))

(define (mzjs a-path)
  (call-with-input-file a-path
    (lambda (ip)
      (call-with-output-file (make-output-path a-path)
        (lambda (op)
          (fprintf op "var _runtime = require('./../../lib');")
          (fprintf op "var program = _runtime.load(~a);" 
                   (jsexp->js (compile-top (zo-parse ip))))
          (fprintf op "_runtime.run(program);"))
        #:exists 'replace))))


(mzjs (command-line #:program "mzjs" 
              #:args (filename)
              filename))