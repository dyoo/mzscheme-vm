#lang s-exp "../lang/base.rkt"

;; big-bang: world (listof handler) -> world
(define (big-bang world . handlers)
  (void))




(define-struct handler (f))


;; to-update-dom: (world dom -> dom) -> handler
(define (to-update-dom f)
  (void))


;; f: world css -> css
(define (to-update-css f)
  (void))


;; stop-when: (world -> boolean) -> handler
(define (stop-when f)
  (void))


;; a-dom: dom
(define (initial-page a-dom)
  (void))

;; a-css: css
(define (initial-css a-css)
  (void))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A query-string is in the form of a jquery query string.

;; dom-find: dom query-string -> (listof node)
(define (dom-find a-dom a-query)
  (void))

;; dom-find-all: dom query-string -> (listof node)
(define (dom-find-all a-dom a-query)
  (void))

;; dom-replace: dom query-string node -> dom
(define (dom-replace a-dom a-query a-replacement)
  (void))

;; node-value: node -> node
(define (node-value a-node)
  (void))

;; node-update-value: node value -> node
(define (node-update-value a-node a-value)
  (void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
