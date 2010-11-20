#lang s-exp "../lang/base.rkt"

(require "jsworld.rkt")

(require (for-syntax racket/base))

(define-syntax (define-effect stx)
  (syntax-case stx ()

    [(_ name (field ...) #:impl impl)
     (syntax/loc stx 
       (define-effect (name #f) (field ...) #:impl impl))]

    [(_ (name supertype) (field ...) #:impl impl)
     (with-syntax ([field-count 
                    (length (syntax->list #'(field ...)))]
                   [struct:name 
                    (datum->syntax #'name 
                                   (string->symbol 
                                    (format "struct:~a"
                                            (syntax-e #'name))))]
                   [name? 
                    (datum->syntax #'name 
                                   (string->symbol 
                                    (format "~a?"
                                            (syntax-e #'name))))]
                   [(field-index ...)
                    (build-list (length #'(field ...)) 
                                (lambda (i) i))]
                   [(accessor ...)
                    (map (lambda (field)
                           (datum->syntax
                            field
                            (string->symbol 
                             (format "~a-~a"
                                     (syntax-e #'name)
                                     (syntax-e field)))))
                         (syntax->list #'(field ...)))]

                   [(mutator ...)
                    (map (lambda (field)
                           (datum->syntax
                            field
                            (string->symbol 
                             (format "set-~a-~a!"
                                     (syntax-e #'name)
                                     (syntax-e field)))))
                         (syntax->list #'(field ...)))])
       
       (syntax/loc stx
         (begin (define-values (struct:name
                                constructor
                                name?
                                name-accessor
                                name-mutator)
                  (make-effect-type 'name 
                                    supertype
                                    field-count
                                    impl))
                (define accessor 
                  (make-struct-field-accessor
                   name-accessor field-index 'field)) ...
                
                (define mutator 
                  (make-struct-field-mutator 
                   name-mutator field-index 'field)) ...
                
                )))]))                                   
    