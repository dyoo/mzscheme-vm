#lang racket/base

(provide record! lookup)

(define-struct record (path permissions))

(define records '())

;; record!: path (listof string) -> void
(define (record! a-path permissions)
  (set! records (cons (make-record a-path permissions)
                      records)))

;; lookup: path -> (listof symbol)
(define (lookup a-path)
  (foldl (lambda (a-record perms)
           (append (record-permissions a-record) perms))
         '()
         (filter (lambda (a-record)
                   (equal? a-path (record-path a-record)))
                 records)))