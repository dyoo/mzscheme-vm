#lang racket/base

(provide record! lookup)

(define-struct record (path permissions))

(define records '())



(define (my-foldl f acc lst)
  (cond
    [(null? lst)
     acc]
    [else
     (my-foldl f (f (car lst) acc) (cdr lst))]))
                    

(define (my-filter f lst)
  (cond
    [(null? lst)
     '()]
    [(f (car lst))
     (cons (car lst) (my-filter f (cdr lst)))]
    [else
     (my-filter f (cdr lst))]))




;; record!: path (listof string) -> void
(define (record! a-path permissions)
  (set! records (cons (make-record a-path permissions)
                      records)))

;; lookup: path -> (listof symbol)
(define (lookup a-path)
  (my-foldl (lambda (a-record perms)
           (append (record-permissions a-record) perms))
         '()
         (my-filter (lambda (a-record)
                   (equal? a-path (record-path a-record)))
                 records)))