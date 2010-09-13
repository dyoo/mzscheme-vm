#lang racket/base

(provide record-implementations! 
         record-exports!
         lookup-implementations
         lookup-exports)

(define-struct record (path impls))
(define-struct export (path exports))

(define records '())
(define exports '())

;; record!: path (listof string) -> void
(define (record-implementations! a-path impls)
  (set! records (cons (make-record a-path impls)
                      records)))


;; record-exports!: path (listof symbol) -> void
(define (record-exports! a-path export-names)
  (set! exports (cons (make-export a-path export-names)
                      exports)))


;; lookup-implementations: path -> (listof string)
(define (lookup-implementations a-path)
  (foldl (lambda (a-record perms)
           (append (record-impls a-record) perms))
         '()
         (filter (lambda (a-record)
                   (equal? a-path (record-path a-record)))
                 records)))


;; lookup-exports: path -> (listof symbol)
(define (lookup-exports a-path)
  (foldl (lambda (an-export exports)
           (append (export-exports an-export) exports))
         '()
         (filter (lambda (an-export)
                   (equal? a-path (export-path an-export)))
                 exports)))