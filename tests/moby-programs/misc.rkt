#lang s-exp "../../lang/wescheme.rkt"

"misc.rkt"

(check-expect (procedure? +) true)
(check-expect (procedure? 1432) false)


(check-expect (pair? 1) false)
(check-expect (pair? empty) false)
(check-expect (pair? '(hello)) true)


(check-expect (cons? 1) false)
(check-expect (cons? empty) false)
(check-expect (cons? '(hello)) true)


(check-expect (empty? 1) false)
(check-expect (empty? empty) true)
(check-expect (empty? '(hello)) false)

(check-expect (null? 1) false)
(check-expect (null? empty) true)
(check-expect (null? '(hello)) false)


(check-expect (undefined? 1) false)
(check-expect (undefined? empty) false)
(check-expect (undefined? '(hello)) false)
(check-expect (undefined? (letrec ([x x]) x)) true)



(check-expect (void? 1) false)
(check-expect (void? empty) false)
(check-expect (void? '(hello)) false)
(check-expect (void? (letrec ([x x]) x)) false)
(check-expect (void? (void)) true)
(check-expect (void? (void (letrec ([x x]) x))) true)


(check-expect (symbol? 'hello) true)
(check-expect (symbol? 3) false)
(check-expect (symbol? "a string") false)

(check-expect (string? 'hello) false)
(check-expect (string? 3) false)
(check-expect (string? "a string") true)


(check-expect (char? 'hello) false)
(check-expect (char? 3) false)
(check-expect (char? "a string") false)
(check-expect (char? #\x) true)


(check-expect (boolean? 'hello) false)
(check-expect (boolean? 3) false)
(check-expect (boolean? "a string") false)
(check-expect (boolean? #\x) false)
(check-expect (boolean? true) true)
(check-expect (boolean? #t) true)
(check-expect (boolean? false) true)
(check-expect (boolean? false) true)

(check-expect (vector? 'hello) false)
(check-expect (vector? 3) false)
(check-expect (vector? "a string") false)
(check-expect (vector? #\x) false)
(check-expect (vector? true) false)
(check-expect (vector? (vector 3 4)) true)
(check-expect (vector? #(hello world)) true)



(define-struct my-struct ())
(check-expect (struct? 'hello) false)
(check-expect (struct? 3) false)
(check-expect (struct? "a string") false)
(check-expect (struct? #\x) false)
(check-expect (struct? true) false)
(check-expect (struct? (vector 3 4)) false)
(check-expect (struct? (make-my-struct)) true)


(check-expect (immutable? '(42)) false)



(check-expect (eof-object? 'hello) false)
(check-expect (eof-object? eof) true)



(check-expect (bytes? 'hello) false)
(check-expect (bytes? 3) false)
(check-expect (bytes? "a string") false)
(check-expect (bytes? #\x) false)
(check-expect (bytes? true) false)
(check-expect (bytes? (vector 3 4)) false)
(check-expect (bytes? (make-my-struct)) false)
(check-expect (bytes? (bytes 1 2 3 4)) true)


(let loop ([i -300])
  (when (< i 300)
    (begin
      (check-expect (byte? i)
                    (and (<= 0 i) (< i 256)))
      (loop (add1 i)))))


(check-expect (number? 'hello) false)
(check-expect (number? 3) true)
(check-expect (number? "a string") false)
(check-expect (number? #\x) false)
(check-expect (number? true) false)
(check-expect (number? (vector 3 4)) false)
(check-expect (number? (make-my-struct)) false)
(check-expect (number? (bytes 1 2 3 4)) false)


(check-expect (complex? 'hello) false)
(check-expect (complex? 3) true)
(check-expect (complex? "a string") false)
(check-expect (complex? #\x) false)
(check-expect (complex? true) false)
(check-expect (complex? (vector 3 4)) false)
(check-expect (complex? (make-my-struct)) false)
(check-expect (complex? (bytes 1 2 3 4)) false)


(check-expect (real? 'hello) false)
(check-expect (real? 3) true)
(check-expect (real? 3+0.0i) false)
(check-expect (real? "a string") false)
(check-expect (real? #\x) false)
(check-expect (real? true) false)
(check-expect (real? (vector 3 4)) false)
(check-expect (real? (make-my-struct)) false)
(check-expect (real? (bytes 1 2 3 4)) false)


(check-expect (rational? 'hello) false)
(check-expect (rational? 3) true)
(check-expect (rational? 3/4) true)
(check-expect (rational? 3.2) true)
(check-expect (rational? 3+0.0i) false)
(check-expect (rational? "a string") false)
(check-expect (rational? #\x) false)
(check-expect (rational? true) false)
(check-expect (rational? (vector 3 4)) false)
(check-expect (rational? (make-my-struct)) false)
(check-expect (rational? (bytes 1 2 3 4)) false)


(check-expect (integer? 'hello) false)
(check-expect (integer? 3) true)
(check-expect (integer? 3/4) false)
(check-expect (integer? 3.2) false)
(check-expect (integer? 3+0.0i) false)
(check-expect (integer? "a string") false)
(check-expect (integer? #\x) false)
(check-expect (integer? true) false)
(check-expect (integer? (vector 3 4)) false)
(check-expect (integer? (make-my-struct)) false)
(check-expect (integer? (bytes 1 2 3 4)) false)


(check-expect (exn:fail:contract? (with-handlers ([void identity]) (odd? 'hello)))
	      true)
(check-expect (odd? 3) true)
(check-expect (odd? 2) false)
(check-expect (exn:fail:contract? (with-handlers ([void identity]) (odd? 3/2)))
	      true)


(check-expect (exn:fail:contract? (with-handlers ([void identity]) (even? 'hello)))
	      true)
(check-expect (even? 3) false)
(check-expect (even? 2) true)
(check-expect (exn:fail:contract? (with-handlers ([void identity]) (even? 3/2)))
	      true)



(check-expect (exn:fail:contract? (with-handlers ([void identity]) (zero? 'hello)))
	      true)
(check-expect (zero? 3) false)
(check-expect (zero? 2) false)
(check-expect (zero? 0) true)
(check-expect (zero? 0.0) true)
(check-expect (zero? 0.0+0.0i) true)
(check-expect (zero? 3/2) false)


(check-expect (positive? 3) true)
(check-expect (positive? 0) false)
(check-expect (positive? -3) false)


(check-expect (negative? 3) false)
(check-expect (negative? 0) false)
(check-expect (negative? -3) true)


(check-expect (box? 3) false)
(check-expect (box? (box 3)) true)


(check-expect (hash? 3) false)
(check-expect (hash? (make-hash)) true)

(check-expect (eq? 'hello 'world) false)
(check-expect (eq? 'hello 'hello) true)
(check-expect (eq? (expt 2 500) (expt 2 500)) false)


(check-expect (eqv? 'hello 'world) false)
(check-expect (eqv? 'hello 'hello) true)
(check-expect (eqv? (expt 2 500) (expt 2 500)) true)
(check-expect (eqv? (expt 2 500) (add1 (expt 2 500))) false)


(check-expect (equal? "hello" "hello") true)
(check-expect (equal? "hello" 17) false)


(check-expect (equal~? "hello" "hello" 0.1) true)
(check-expect (equal~? 16 17 1) true)
(check-expect (equal~? 16 17 .1) false)
(check-expect (exn:fail:contract? 
	       (with-handlers ([void identity]) (equal~? 16 17 'foo)))
	      true)


(check-expect (false? false) true)
(check-expect (false? true) false)
(check-expect (false? 3) false)


(check-expect (boolean=? false true) false)
(check-expect (boolean=? false false) true)
(check-expect (boolean=? true true) true)
(check-expect (boolean=? true false) false)
(check-expect (exn:fail:contract?
	       (with-handlers ([void identity])
		  (boolean=? 3 false)))
	      true)



(check-expect (exn:fail:contract?
	       (with-handlers ([void identity])
                 (symbol=? false 'false)))
	      true)
(check-expect (symbol=? 'hello 'world) false)
(check-expect (symbol=? 'hello 'hello) true)


(check-expect 
 (call-with-current-continuation
  (lambda (return)
    (return 42)
    (error 'should-not-be-here)))
 42)



(check-expect (ormap even? '(1 3 5 7 9)) #f)
(check-expect (ormap even? '(1 3 5 8 9)) true)


(check-expect (continuation-prompt-tag?
	        (make-continuation-prompt-tag)) #t)


(check-expect (string->symbol "a-symbol") 'a-symbol)


(check-expect (append '(1 2 3) 4)
	      (cons 1 (cons 2 (cons 3 4))))

(check-expect (append '(1 2 3) '(4))
	      (list 1 2 3 4))


(check-expect (list-ref '(a e i o u) 0) 'a)
(check-expect (list-ref '(a e i o u) 1) 'e)
(check-expect (list-ref '(a e i o u) 2) 'i)
(check-expect (list-ref '(a e i o u) 3) 'o)
(check-expect (list-ref '(a e i o u) 4) 'u)
(check-expect (exn:fail:contract? 
	       (with-handlers ([void identity])
		 (list-ref '(a e i o u) 5)))
	      true)
(check-expect (exn:fail:contract? 
	       (with-handlers ([void identity])
		 (list-ref '(a e i o u) -1)))
	      true)


(check-expect (memq 2 (list 1 2 3 4)) '(2 3 4))
(check-expect (memq 9 (list 1 2 3 4)) #f)


(check-expect (assv 3 (list (list 1 2) (list 3 4) (list 5 6)))
	      '(3 4))

(check-expect (cdar '((7 6 5 4 3 2 1) 8 9))
	      '(6 5 4 3 2 1))
(check-expect (cadr '((1 2) 3 4))
	      3)
(check-expect (caar '((1 2) 3 4))
	       1)

(check-expect (cddr '(2 1))
	      '())

(check-expect (caaar '(((6 5 4 3 2 1) 7) 8 9))
	      6)

(check-expect (caadr '(9 (7 6 5 4 3 2 1) 8))
	      7)

(check-expect (cadar '((7 6 5 4 3 2 1) 8 9))
	      6)
(check-expect (caddr '(3 2 1))
	      1)

(check-expect (cdaar '(((6 5 4 3 2 1) 7) 8 9))
	      '(5 4 3 2 1))

(check-expect (cdadr '(9 (7 6 5 4 3 2 1) 8))
	      '(6 5 4 3 2 1))

(check-expect (cddar '((7 6 5 4 3 2 1) 8 9))
	      '(5 4 3 2 1))

(check-expect (cdddr '(3 2 1))
	      '())
(check-expect (cadddr '(4 3 2 1))
	      1)


(check-expect (list? empty) true)
(check-expect (list? '(1 2)) true)
(check-expect (list? '(1 . 2)) false)


(let ([ht (make-hash)])
  (hash-set! ht 'name "danny")
  (check-expect (hash-ref ht 'name)
		"danny")
  (check-expect (hash-map ht (lambda (k v) (list k v)))
		'((name "danny"))))



(let* ([holder (make-placeholder #f)]
       [template `(hello world ,holder)])
  (check-expect (make-reader-graph template)
		'(hello world #f))
  (placeholder-set! holder "test")
  (check-expect (make-reader-graph template)
		'(hello world "test")))


(check-expect (exact? #i3.42) #f)
(check-expect (exact? 3) #t)
(check-expect (exn:fail:contract? (with-handlers ([void identity]) (exact? "not a number"))) true)



(check-expect (log 1) 0)
(check-within (log 6) 1.791759469228055 0.0001)
(check-within (tan 1) 1.5574077246549023 0.0001)
(check-expect (acos 1) 0)


(check-expect (string->int "3") 51)


"misc.rkt end"