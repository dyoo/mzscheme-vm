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

                     cond
                     if
                     case
                     ))

(require (for-syntax racket/base
                     syntax/stx))


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







(define-syntax (-if stx)
  (syntax-case stx ()
    [(_ test then else)
     (with-syntax ([new-test (syntax (verify-boolean test 'if))])
       (syntax/loc stx
         (if new-test
             then
             else)))]
    [(_ . rest)
     (let ([n (length (syntax->list (syntax rest)))])
       (teach-syntax-error
        'if
        stx
        #f
        "expected one question expression and two answer expressions, but found ~a expression~a"
        (if (zero? n) "no" n)
        (if (= n 1) "" "s")))]
    [_else (bad-use-error 'if stx)]))

(provide (rename-out [-if if]))




;; Use to generate nicer error messages than direct pattern
;; matching. The `where' argument is an English description
;; of the portion of the larger expression where a single
;; sub-expression was expected.
(define-for-syntax (check-single-expression who where stx exprs will-bind)
  (when (null? exprs)
    (teach-syntax-error
     who
     stx
     #f
     "expected an expression ~a, but nothing's there"
     where))
  (unless (null? (cdr exprs))
    ;; In case it's erroneous, to ensure left-to-right reading, let's
    ;;  try expanding the first expression. We have to use
    ;;  `will-bind' to avoid errors for unbound ids that will actually
    ;;  be bound. Since they're used as stopping points, we may miss
    ;;  some errors after all. It's worth a try, though. We also
    ;;  have to stop at advanced-set!, in case it's used with
    ;;  one of the identifiers in will-bind.
    (when will-bind
      (local-expand-for-error (car exprs) 'expression (cons #'advanced-set!
                                                            will-bind)))
    ;; First expression seems ok, report an error for 2nd and later:
    (teach-syntax-error
     who
     stx
     (cadr exprs)
     "expected only one expression ~a, but found ~a extra part"
     where
     (if (null? (cddr exprs))
         "one"
         "at least one"))))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; case
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (-case stx)
  (syntax-case stx ()
    [(_)
     (teach-syntax-error
      'case
      stx
      #f
      "expected an expression after `case', but nothing's there")]
    [(_ expr)
     (teach-syntax-error
      'case
      stx
      #f
      "expected a choices--answer clause after the expression following `case', but nothing's there")]
    [(_ v-expr clause ...)
     (let ([clauses (syntax->list (syntax (clause ...)))])
       (for-each
        (lambda (clause)
          (syntax-case clause (else)
            [(else answer ...)
             (let ([lpos (memq clause clauses)])
               (when (not (null? (cdr lpos)))
                 (teach-syntax-error
                  'case
                  stx
                  clause
                  "found an `else' clause that isn't the last clause ~
                                    in its `case' expression"))
               (let ([answers (syntax->list (syntax (answer ...)))])
                 (check-single-expression 'case
                                          "for the answer in a case clause"
                                          clause
                                          answers
                                          null)))]
            [(choices answer ...)
             (let ([choices (syntax choices)]
                   [answers (syntax->list (syntax (answer ...)))])
               (syntax-case choices ()
                 [(elem ...)
                  (let ([elems (syntax->list (syntax (elem ...)))])
                    (for-each (lambda (e)
                                (let ([v (syntax-e e)])
                                  (unless (or (number? v)
                                              (symbol? v))
                                    (teach-syntax-error
                                     'case
                                     stx
                                     e
                                     "expected a name (for a symbol) or a number as a choice value, but found ~a"
                                     (something-else e)))))
                              elems))]
                 [_else (teach-syntax-error
                         'case
                         stx
                         choices
                         "expected a parenthesized sequence of choice values, but found ~a"
                         (something-else choices))])
               (when (stx-null? choices)
                 (teach-syntax-error
                  'case
                  stx
                  choices
                  "expected at least once choice in a parenthesized sequence of choice values, but nothing's there"))
               (check-single-expression 'case
                                        "for the answer in a `case' clause"
                                        clause
                                        answers
                                        null))]
            [()
             (teach-syntax-error
              'case
              stx
              clause
              "expected a choices--answer clause, but found an empty clause")]
            [_else
             (teach-syntax-error
              'case
              stx
              clause
              "expected a choices--answer clause, but found ~a"
              (something-else clause))]))
        clauses)
       ;; Add `else' clause for error, if necessary:
       (let ([clauses (let loop ([clauses clauses])
                        (cond
                          [(null? clauses)
                           (list
                            (syntax/loc stx
                              [else (error 'case "the expression matched none of the choices")]))]
                          [(syntax-case (car clauses) (else)
                             [(else . _) (syntax/loc (car clauses) (else . _))]
                             [_else #f])
                           => 
                           (lambda (x) (cons x (cdr clauses)))]
                          [else (cons (car clauses) (loop (cdr clauses)))]))])
         (with-syntax ([clauses clauses])
           (syntax/loc stx (case v-expr . clauses)))))]
    [_else (bad-use-error 'case stx)]))

(provide (rename-out [-case case]))












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