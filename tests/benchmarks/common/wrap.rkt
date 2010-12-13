#lang s-exp "../../../lang/base.rkt"
(provide (rename-out (module-begin #%module-begin))
         (except-out (all-from-out "../../../lang/base.rkt")
                     #%module-begin)
         with-input-from-file)


(require (prefix-in base: "../../../lang/base.rkt")
         (for-syntax racket/base))


;; mock for with-input-from-file: just have it return false.
(define-syntax (with-input-from-file stx)
  (syntax-case stx ()
    [(_ name f)
     (syntax/loc stx
       eof)]))

(define-syntax (module-begin stx)
  (define name (syntax-property stx 'enclosing-module-name))
  (define content (call-with-input-file
                      (format "~a.sch" name)
                    (lambda (ip)
                      (let loop ()
                        (let ([next-stx 
                               (read ip)])
                          (cond
                            [(eof-object? next-stx)
                             '()]
                            [else
                             (cons next-stx (loop))]))))))  
  (with-syntax ([(content ...) content])
    #`(base:#%module-begin
       content ...)))


