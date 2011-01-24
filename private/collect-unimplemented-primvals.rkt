#lang s-exp "profiled-base.rkt"
(require racket/match
         racket/bool
         racket/list
         racket/contract
         "jsexp.rkt"
         "get-implemented-primitives.rkt")

(provide/contract [collect-unimplemented-primvals (jsexp? . -> . (listof symbol?))])


(define known-primvals (make-hash))
(for ([name primitive-names])
  (hash-set! known-primvals name #t))


;; jsexp->js: jsexp -> (listof symbol)
;; Given a jsexp, walk through its structure and collect the primvals that we
;; have not yet implemented.
(define (collect-unimplemented-primvals a-jsexp)
  (let ([used-primvals (primvals-in-jsexp a-jsexp)])
    (filter (lambda (p)
              (not (hash-ref known-primvals p #f)))
            used-primvals)))


;; primvals-in-jsexp: jsexp -> (listof symbol)
;; Produces a list of the primitives used in the program.
(define (primvals-in-jsexp a-jsexp)
  (let ([seen-primvals (make-hash)])
    (let walk ([a-jsexp a-jsexp])
      (match a-jsexp
        [(struct ht (name pairs))
         (cond
           [(symbol=? name 'primval)
            (let ([prim-sym 
                   (string->symbol (lit-v (second (first pairs))))])
              (hash-set! seen-primvals prim-sym #t))]
           [else
            (for ([p pairs])
              (walk (second p)))])]
        [(struct vec (items))
         (for-each walk items)]
        [(struct int (v))
         (void)]
        [(struct lit (v))
         (void)]))
  
    (for/list ([key (in-hash-keys seen-primvals)])
      key)))