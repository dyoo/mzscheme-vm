#lang racket/base

(provide record-javascript-implementation!
         lookup-javascript-implementation)


(define-struct record (path impl))
(define records '())

;; record-javascript-implementation!: path module-path -> void
(define (record-javascript-implementation! a-path an-impl)
  (set! records (cons (make-record a-path an-impl)
                      records)))


(define (find path lst)
  (cond
    [(null? lst)
     (error 'find "Couldn't find ~s" path)]
    [(equal? path (record-path (car lst)))
     (car lst)]
    [else
     (find path (cdr lst))]))


;; lookup-javascript-implementation: path -> module-path
(define (lookup-javascript-implementation a-path)
  (record-impl (find a-path records)))
