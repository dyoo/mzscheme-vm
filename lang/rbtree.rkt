#lang s-exp "base.rkt"

;; Red black trees.
;; Most of this comes from the code in:
;; http://programmingpraxis.com/2009/10/02/red-black-trees/2/
(define-struct rbnode (color key value lkid rkid))

(define empty-rbnode
  (make-rbnode 'black 'nil 'nil 'nil 'nil))

;; rbnode-empty?: (treeof X Y) -> boolean
(define (rbnode-empty? t) 
  (eq? t empty-rbnode))

;; red?: color -> boolean
(define (rbnode-color-red? c) 
  (eq? c 'red))

;; black?: color -> boolean
(define (rbnode-color-black? c) 
  (eq? c 'black))


;; rbnode-lookup: (X X -> boolean) (treeof X Y) X -> (or/c false (list X Y))
(define (rbnode-lookup lt? t k)
  (cond [(rbnode-empty? t) 
         #f]
        [(lt? k (rbnode-key t)) 
         (rbnode-lookup lt? (rbnode-lkid t) k)]
        [(lt? (rbnode-key t) k)
         (rbnode-lookup lt? (rbnode-rkid t) k)]
        [else 
         (list (rbnode-key t) (rbnode-value t))]))



;; rbnode-insert: (X X -> boolean) (treeof X Y) X Y -> (treeof X Y)
(define (rbnode-insert lt? t k v)
  (local [(define (ins t)
            (cond [(rbnode-empty? t) (make-rbnode 'red k v empty-rbnode empty-rbnode)]
                  [(lt? k (rbnode-key t))
                   (rbnode-balance (rbnode-color t) (rbnode-key t) (rbnode-value t) (ins (rbnode-lkid t)) (rbnode-rkid t))]
                  [(lt? (rbnode-key t) k)
                   (rbnode-balance (rbnode-color t) (rbnode-key t) (rbnode-value t) (rbnode-lkid t) (ins (rbnode-rkid t)))]
                  [else
                   (make-rbnode (rbnode-color t) k v (rbnode-lkid t) (rbnode-rkid t))]))]
    (let ([z (ins t)])
      (make-rbnode 'black (rbnode-key z) (rbnode-value z) (rbnode-lkid z) (rbnode-rkid z)))))


;; rbnode-balance: color X Y (treeof X Y) (treeof X Y) -> (treeof X Y)
(define (rbnode-balance c k v l r)
  (cond [(and (rbnode-color-black? c) (rbnode-color-red? (rbnode-color l)) (rbnode-color-red? (rbnode-color (rbnode-lkid l))))
         (make-rbnode 'red (rbnode-key l) (rbnode-value l)
                      (make-rbnode 'black (rbnode-key (rbnode-lkid l)) (rbnode-value (rbnode-lkid l))
                                   (rbnode-lkid (rbnode-lkid l)) (rbnode-rkid (rbnode-lkid l)))
                      (make-rbnode 'black k v (rbnode-rkid l) r))]
        [(and (rbnode-color-black? c) (rbnode-color-red? (rbnode-color l)) (rbnode-color-red? (rbnode-color (rbnode-rkid l))))
         (make-rbnode 'red (rbnode-key (rbnode-rkid l)) (rbnode-value (rbnode-rkid l))
                      (make-rbnode 'black (rbnode-key l) (rbnode-value l) (rbnode-lkid l) (rbnode-lkid (rbnode-rkid l)))
                      (make-rbnode 'black k v (rbnode-rkid (rbnode-rkid l)) r))]
        [(and (rbnode-color-black? c) (rbnode-color-red? (rbnode-color r)) (rbnode-color-red? (rbnode-color (rbnode-lkid r))))
         (make-rbnode 'red (rbnode-key (rbnode-lkid r)) (rbnode-value (rbnode-lkid r))
                      (make-rbnode 'black k v l (rbnode-lkid (rbnode-lkid r)))
                      (make-rbnode 'black (rbnode-key r) (rbnode-value r) (rbnode-rkid (rbnode-lkid r)) (rbnode-rkid r)))]
        [(and (rbnode-color-black? c) (rbnode-color-red? (rbnode-color r)) (rbnode-color-red? (rbnode-color (rbnode-rkid r))))
         (make-rbnode 'red (rbnode-key r) (rbnode-value r)
                      (make-rbnode 'black k v l (rbnode-lkid r))
                      (make-rbnode 'black (rbnode-key (rbnode-rkid r)) (rbnode-value (rbnode-rkid r))
                                   (rbnode-lkid (rbnode-rkid r)) (rbnode-rkid (rbnode-rkid r))))]
        [else (make-rbnode c k v l r)]))


;; rbnode-fold: rbnode (X Y Z -> Z) Z -> Z
;; Folds a function across all the key/value pairs in the tree.
(define (rbnode-fold t folding-function acc)
  (cond
    [(rbnode-empty? t)
     acc]
    [else
     (folding-function (rbnode-key t)
                       (rbnode-value t)
                       (rbnode-fold (rbnode-rkid t) 
                                    folding-function
                                    (rbnode-fold (rbnode-lkid t) folding-function acc)))]))



;; rbnode-keys: rbnode -> (listof keys)
;; Produces all of the keys in the rbnode.
(define (rbnode-keys t)
  (rbnode-fold t 
               (lambda (key value keys) (cons key keys))
               '()))
               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rbtrees are a convenient wrapper around rbnodes that remember their
;; comparison operator.  They also provide a delete operation, where delete
;; operation replaces the value with a DELETED-SENTINEL value.


(define-struct rbtree (root cmp))
(define-struct deleted-sentinel ())
(define DELETED-SENTINEL (make-deleted-sentinel))


;; empty-rbtree: (X X -> boolean) -> rbtreeof(X, Y)
;; Creates an empty rbtree.
(define (empty-rbtree cmp)
  (make-rbtree empty-rbnode cmp))

;; rbtree-set: rbtreeof(X, Y) X Y -> rbtreeof(X, Y)
;; Functionally add a key/value association.
;; If one already exists, replaces it.
(define (rbtree-set a-rbtree k v)
  (make-rbtree (rbnode-insert (rbtree-cmp a-rbtree)
                              (rbtree-root a-rbtree)
                              k 
                              v)
               (rbtree-cmp a-rbtree)))


;; rbtree-remove: rbtreeof(X, Y) X -> rbtreeof(X, Y)
;; Functionally remove the key/value association.
(define (rbtree-remove a-rbtree k)
  (make-rbtree (rbnode-insert (rbtree-cmp a-rbtree)
                              (rbtree-root a-rbtree)
                              k 
                              DELETED-SENTINEL)
               (rbtree-cmp a-rbtree)))

;; rbtree-has-key?: rbtreeof(X, Y) X -> boolean
;; Check to see if a key is in the tree.
(define (rbtree-has-key? a-rbtree k)
  (let ([node/false (rbnode-lookup (rbtree-cmp a-rbtree)
                                   (rbtree-root a-rbtree)
                                   k)])
    (cond
      [(false? node/false)
       #f]
      [(eq? (second node/false) DELETED-SENTINEL)
       #f]
      [else
       #t])))

;; rbtree-ref: rbtreeof(X, Y) X -> Y
;; rbtree-ref: rbtreeof(X, Y) X (-> Y) -> Y
;;
;; Look up an element in the rbtree.
(define rbtree-ref
  (case-lambda [(a-rbtree k on-fail)
                (let ([node/false (rbnode-lookup (rbtree-cmp a-rbtree)
                                                 (rbtree-root a-rbtree)
                                                 k)])
                  (cond
                    [(false? node/false)
                     (if (procedure? on-fail) (on-fail) on-fail)]
                    
                    [(eq? (second node/false) DELETED-SENTINEL)
                     (if (procedure? on-fail) (on-fail) on-fail)]
                    [else
                     (second node/false)]))]
               [(a-rbtree k)
                (rbtree-ref a-rbtree k (lambda () (error 'rbtree-ref 
                                                         "no value found for key: ~e" k)))]))

;; rbtree-keys: rbtreeof(X, Y) -> (listof X)
;; Produces the keys in the tree.
(define (rbtree-keys a-rbtree)
  (rbnode-fold (rbtree-root a-rbtree)
               (lambda (key value keys) 
                 (cond [(eq? value DELETED-SENTINEL)
                        keys]
                       [else
                        (cons key keys)]))
               '()))


;; rbtree-fold: rbtreeof(X, Y) (X Y Z -> Z) Z -> Z
;; Folds a function across the key/value pairs of the rbtree.
(define (rbtree-fold a-rbtree f acc)
  (rbnode-fold (rbtree-root a-rbtree)
               (lambda (key value acc) 
                 (cond [(eq? value DELETED-SENTINEL)
                        acc]
                       [else
                        (f key value acc)]))
               acc))








(provide rbtree?
	 empty-rbtree
	 rbtree-set
         rbtree-remove
	 rbtree-has-key?
	 rbtree-ref
         rbtree-keys
         rbtree-fold)
