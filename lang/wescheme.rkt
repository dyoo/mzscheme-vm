#lang s-exp "base.rkt"

;; This is the default language module for WeScheme programs.
;;
;; It implements a "pretty-big"-like language that'll expose everything
;; in one big module, just so users don't have to worry about
;; doing things with require.


(provide (except-out (all-from-out "base.rkt")
                     
                     ;; We provide everything from base except
                     ;; syntax stuff, because it's not quite safe
                     ;; to do arbitrary server-side compilation.
                     define-syntax
                     define-for-syntax
                     begin-for-syntax
                     for-syntax
		     define-struct
		     cond))

(require (for-syntax racket/base))


(provide (rename-out [-define-struct define-struct]))
(define-syntax (-define-struct stx)
  (syntax-case stx ()
    [(_ struct-id (field-id ...))
     (syntax/loc stx
       (define-struct struct-id (field-id ...) #:mutable))]))


#;(define-syntax (-cond stx)
  (syntax-case stx ()
    [(_ clauses ...)
     (let ([clauses (syntax->list #'(clauses ...))])
       (with-syntax ([(updated-clauses ...)
                      clauses])
         (syntax/loc stx
           (cond updated-clauses ...))))]))
(provide (rename-out [-cond cond]))






(define-for-syntax (local-expand-for-error stx ctx stops)
  ;; This function should only be called in an 'expression
  ;;  context. In case we mess up, avoid bogus error messages.
  (when (memq (syntax-local-context) '(expression))
    (local-expand stx ctx stops)))



    


;; Raise a syntax error:
(define-for-syntax (teach-syntax-error form stx detail msg . args)
  (let ([form (if (eq? form '|function call|)
                  form
                  #f)] ; extract name from stx
        [msg (apply format msg args)])
    (if detail
        (raise-syntax-error form msg stx detail)
        (raise-syntax-error form msg stx))))

(define-for-syntax (teach-syntax-error* form stx details msg . args)
  (let ([exn (with-handlers ([exn:fail:syntax?
                              (lambda (x) x)])
               (apply teach-syntax-error form stx #f msg args))])
    (raise
     (make-exn:fail:syntax
      (exn-message exn)
      (exn-continuation-marks exn)
      details))))



;; The syntax error when a form's name doesn't follow a "("
(define-for-syntax (bad-use-error name stx)
  (teach-syntax-error
   name
   stx
   #f
   "found a use of `~a' that does not follow an open parenthesis"
   name))

(define-for-syntax (something-else v)
  (let ([v (syntax-e v)])
    (cond
      [(number? v) "a number"]
      [(string? v) "a string"]
      [else "something else"])))

;; verify-boolean is inserted to check for boolean results:
(define (verify-boolean b where)
  (if (or (eq? b #t) (eq? b #f))
      b
      (raise
       (make-exn:fail:contract
        (format "~a: question result is not true or false: ~e" where b)
        (current-continuation-marks)))))

(define-syntax (-cond stx)
  (syntax-case stx ()
    [(_)
     (teach-syntax-error
      'cond
      stx
      #f
      "expected a question--answer clause after `cond', but nothing's there")]
    [(_ clause ...)
     (let* ([clauses (syntax->list (syntax (clause ...)))]
            [check-preceding-exprs
             (lambda (stop-before)
               (let/ec k
                 (for-each (lambda (clause)
                             (if (eq? clause stop-before)
                                 (k #t)
                                 (syntax-case clause ()
                                   [(question answer)
                                    (begin
                                      (unless (and (identifier? (syntax question))
                                                   (free-identifier=? (syntax question)
                                                                      #'else))
                                        (local-expand-for-error (syntax question) 'expression null))
                                      (local-expand-for-error (syntax answer) 'expression null))])))
                           clauses)))])
       (let ([checked-clauses
              (map
               (lambda (clause)
                 (syntax-case clause (else)
                   [(else answer)
                    (let ([lpos (memq clause clauses)])
                      (when (not (null? (cdr lpos)))
                        (teach-syntax-error
                         'cond
                         stx
                         clause
                         "found an `else' clause that isn't the last clause ~
                                    in its `cond' expression"))
                      (with-syntax ([new-test (syntax #t) ])
                        (syntax/loc clause (new-test answer))))]
                   [(question answer)
                    (with-syntax ([verified 
                                   (syntax (verify-boolean question 'cond))])
                      (syntax/loc clause (verified answer)))]
                   [()
                    (check-preceding-exprs clause)
                    (teach-syntax-error
                     'cond
                     stx
                     clause
                     "expected a question--answer clause, but found an empty clause")]
                   [(question?)
                    (check-preceding-exprs clause)
                    (teach-syntax-error
                     'cond
                     stx
                     clause
                     "expected a clause with a question and answer, but found a clause with only one part")]
                   [(question? answer? ...)
                    (check-preceding-exprs clause)
                    (let ([parts (syntax->list clause)])
                      ;; to ensure the illusion of left-to-right checking, make sure 
                      ;; the question and first answer (if any) are ok:
                      (unless (and (identifier? (car parts))
                                   (free-identifier=? (car parts) #'else))
                        (local-expand-for-error (car parts) 'expression null))
                      (unless (null? (cdr parts))
                        (local-expand-for-error (cadr parts) 'expression null))
                      ;; question and answer (if any) are ok, raise a count-based exception:
                      (teach-syntax-error*
                       'cond
                       stx
                       parts
                       "expected a clause with one question and one answer, but found a clause with ~a parts"
                       (length parts)))]
                   [_else
                    (teach-syntax-error
                     'cond
                     stx
                     clause
                     "expected a question--answer clause, but found ~a"
                     (something-else clause))]))
               clauses)])
         ;; Add `else' clause for error (always):
         (let ([clauses (append checked-clauses 
                                (list 
                                 (with-syntax ([error-call (syntax/loc stx (error 'cond "all question results were false"))])
                                   (syntax [else error-call]))))])
           (with-syntax ([clauses clauses])
             (syntax/loc stx (cond . clauses))))))]
    [_else (bad-use-error 'cond stx)]))


















;; Pull in the image primitives too.
(require "../image/image.rkt")
(provide (all-from-out "../image/image.rkt"))

;; For compatibility, re-exports image-url as open-image-url.
(define open-image-url image-url)
(provide open-image-url)

;; Also re-export js-big-bang as big-bang.
(define js-big-bang big-bang)
(provide js-big-bang)

;; re-export jsworld
(require "../jsworld/jsworld.rkt")
(provide (all-from-out "../jsworld/jsworld.rkt"))


;; re-export posn
(require "posn.rkt")
(provide (all-from-out "posn.rkt"))


;; re-export check-expect
(require "check-expect/check-expect.rkt")
(provide (all-from-out "check-expect/check-expect.rkt"))

;; re-export shared
(require "../shared.rkt")
(provide (all-from-out "../shared.rkt"))