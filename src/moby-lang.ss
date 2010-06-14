#lang scheme/base

(require scheme/base
         scheme/bool)

(require (prefix-in base: (for-syntax scheme/base))
	 (prefix-in kernel: (for-syntax '#%kernel)))


(define-syntax (provide-stub stx)
  (syntax-case stx ()
    [(_ (name . args))
     (syntax/loc stx
       (begin
	 (define (name . args)
	   'stub)
	 (provide name)))]))




(define-syntax (module-begin stx)
  (if (eq? 'module-begin (syntax-local-context))
      (void)
      (raise-syntax-error
       #f
       "allowed only around a module body"
       stx))
  (if (symbol? (syntax-e stx))
      (raise-syntax-error
       #f
       "bad syntax" 
       stx)
      (void))
  (let-values ([(l) (syntax->list stx)])
	      (if l
		  (void)
		  (raise-syntax-error
		   #f
		   "bad syntax (illegal use of `.')" 
		   stx))
	      (datum->syntax
	       stx
	       (cons (quote-syntax kernel:#%module-begin)
		     (cdr l))
	       stx
	       stx)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide [rename-out (module-begin #%module-begin)])
(provide [rename-out (base:#%app #%app)])
(provide [rename-out (base:#%datum #%datum)])
(provide [rename-out (base:#%top #%top)])
(provide define)
(provide define-struct)


(provide true false)

(provide +)
(provide >=)
(provide /)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(provide-stub (place-image img x y scene))
(provide-stub (circle size shape color))
(provide-stub (empty-scene width height))
(provide-stub (js-big-bang ...))
(provide-stub (on-tick ...))
(provide-stub (on-redraw ...))
(provide-stub (stop-when ...))
(provide-stub (check-expect ...))

