#lang s-exp "../../lang/wescheme.rkt"

"misc.rkt"

;; The tests here make sure that all of the functions
;; that we provide are at least exercised once.
;; They are not meant to be systematic.



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


(check-expect 
 (call/cc
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


(check-expect (memv 2 (list 1 2 3 4))
	      '(2 3 4))
(check-expect (memv 9 (list 1 2 3 4))
	      #f)


(check-expect (assoc 3 (list (list 1 2) (list 3 4) (list 5 6)))
	      '(3 4))
(check-expect (assoc 9 (list (list 1 2) (list 3 4) (list 5 6)))
	      #f)


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
(check-expect (cos 0) 1)
(check-within (cos 1) 0.5403023058681398 0.0001)
(check-expect (acos 1) 0)
(check-within (acos 0) 1.5707963267948966 0.0001)

(check-expect (magnitude 5) 5)
(check-expect (magnitude 0+5i) 5)
(check-within (magnitude 5+5i) 7.0710678118654755 0.0001)

(check-expect (string->int "3") 51)


(check-expect (string-upper-case? "hello") false)
(check-expect (string-upper-case? "Hello") false)
(check-expect (string-upper-case? "HELLO") true)

(check-expect (string-lower-case? "hello") true)
(check-expect (string-lower-case? "Hello") false)
(check-expect (string-lower-case? "HELLO") false)


(check-expect (string-length "") 0)
(check-expect (string-length "abcdefghijklmnopqrstuvwxyz") 26)


(check-expect (string-ith "wpi" 0) "w")
(check-expect (string-ith "wpi" 1) "p")
(check-expect (string-ith "wpi" 2) "i")
(check-expect (exn:fail:contract?
	       (with-handlers ([void identity])
		 (string-ith "wpi" 3)))
	      true)
(check-expect (exn:fail:contract?
	       (with-handlers ([void identity])
		 (string-ith "wpi" -1)))
	      true)

(let ([p (make-posn 3 4)])
  (check-expect (posn-x p) 3)
  (set-posn-x! p 17)
  (check-expect (posn-x p) 17)
  (check-expect p (make-posn 17 4))
  (set-posn-y! p -23)
  (check-expect p (make-posn 17 -23)))


(check-expect (replicate 3 "hi") "hihihi")
(check-expect (replicate 0 "hi") "")

(check-expect (number->string 42) "42")
(check-expect (number->string -42) "-42")
(check-expect (number->string -0.0) "-0.0")
(check-expect (number->string +inf.0) "+inf.0")
(check-expect (number->string -inf.0) "-inf.0")
(check-expect (number->string 3/4) "3/4")


(check-expect (implode '("a" "b" "c")) "abc")


(check-expect (string->number "42") 42)
(check-expect (string->number "-42") -42) 
(check-expect (string->number "-0.0") -0.0) 
(check-expect (string->number "+inf.0") +inf.0) 
(check-expect (string->number "-inf.0") -inf.0) 
(check-expect (string->number "3/4") 3/4) 

(check-expect (symbol->string 'hello-again) "hello-again")


(check-expect (list-tail '(a b c d e) 3)
	      '(d e))
(check-expect (list-tail (list 1 2 3 4) 2)
	      '(3 4))


(check-expect (exn:fail:contract? 
	       (with-handlers ([void identity])
		  (list-tail (list 1 2 3 4) 20)))
	      true)


(check-expect (member 2 (list 1 2 3 4))
	      '(2 3 4))
(check-expect (member 9 (list 1 2 3 4))
	      #f)


(let ([b (box-immutable 42)])
  (check-expect (unbox b) 42)
  (check-expect (exn:fail:contract?
		 (with-handlers ([void identity])
		   (set-box! b 16)))
		true))



(let ([ht (make-hasheq)])
  (hash-set! ht 'name "danny")
  (check-expect (hash-ref ht 'name)
		"danny")
  (check-expect (hash-map ht (lambda (k v) (list k v)))
		'((name "danny")))
  (hash-remove! ht 'name)
  (check-expect (hash-map ht (lambda (k v) (list k v)))
		'()))



(check-expect (inexact? 42) false)
(check-expect (inexact? 22/7) false)
(check-expect (inexact? pi) true)
(check-expect (inexact? e) true)


(check-expect (numerator 22/7) 22)
(check-expect (denominator 22/7) 7)

(check-expect (numerator 234) 234)

(check-expect (integer-sqrt 4) 2)


(check-expect (make-rectangular 3 4)
	      3+4i)



(check-within (exp 3)
	      #i20.08553692318767
	      0.0001)


(check-expect (angle 2984) 0)
(check-expect (angle #i0.0) 0)
(check-expect (angle #i0.234) 0)
(check-within (angle 1+5i) 1.373400766945016 0.00001)




(let ([ht (make-hasheq)]
      [l '()])
  (hash-set! ht 'name "danny")
  (check-expect (hash-ref ht 'name)
		"danny")
  (hash-for-each ht (lambda (k v) (set! l (cons (list k v) l))))
  (check-expect l '((name "danny"))))

(check-expect (string-numeric? "928173419") true)
(check-expect (string-numeric? "") true)
(check-expect (string-numeric? "x") false)



(check-expect (string>? "Apple" "apple")
	      #f)
(check-expect (string>? "apple" "Apple")
	      #t)
(check-expect (string>? "c" "b" "a")
	      #t)

(check-expect (string>=? "Apple" "apple")
	      #f)
(check-expect (string>=? "apple" "Apple")
	      #t)
(check-expect (string>=? "c" "b" "b")
	      #t)

(check-expect (string-ci=? "Apple" "apple")
	      #t)
(check-expect (string-ci=? "a" "a" "a")
	      #t)


(check-expect (string-ci<? "Apple" "apple")
	      #f)
(check-expect (string-ci<? "apple" "banana")
	      #t)
(check-expect (string-ci<? "a" "b" "c")
	      #t)


(check-expect (string-ci<=? "Apple" "apple")
	      #t)
(check-expect (string-ci<=? "apple" "Apple")
	      #t)
(check-expect (string-ci<=? "a" "b" "b")
	      #t)


(check-expect (string-ci>? "Apple" "apple")
	      #f)
(check-expect (string-ci>? "banana" "Apple")
	      #t)
(check-expect (string-ci>? "c" "b" "a")
	      #t)


(check-expect (string-ci>=? "Apple" "apple")
	      #t)
(check-expect (string-ci>=? "apple" "Apple")
	      #t)
(check-expect (string-ci>=? "c" "b" "b")
	      #t)


(check-expect (string->list "Apple")
	      '(#\A #\p #\p #\l #\e))
(check-expect (list->string (list #\A #\p #\p #\l #\e))
	      "Apple")


(check-expect (build-string 5 (lambda (i) (integer->char (+ i 97))))
	      "abcde")

(check-expect (string-append "Apple" "Banana")
	      "AppleBanana")

(let ([s (string #\A #\p #\p #\l #\e)])
  (string-fill! s #\q)
  (check-expect s "qqqqq"))

(check-expect (substring "Apple" 1 3)
	      "pp")
(check-expect (substring "Apple" 1)
	      "pple")



(check-expect (bytes=? #"Apple" #"apple")
	      #f)

(check-expect (bytes=? #"Apple" #"Apple")
	      #t)
(check-expect (bytes=? #"a" #"as" #"a")
	      #f)

(check-expect (bytes->list #"Apple")
	      '(65 112 112 108 101))



(check-expect (truncate 17/4)
	      4)
(check-expect (truncate -17/4)
	      -4)
(check-expect (truncate #i2.5)
	      #i2.0)
(check-expect (truncate #i-2.5)
	      #i-2.0)


(check-expect (eq? js-big-bang big-bang) #t)

(check-expect (bytes-append #"Apple" #"Banana")
	      #"AppleBanana")

(check-expect (bytes=? #"Apple" #"apple")
	      #f)
(check-expect (bytes=? #"a" #"as" #"a")
	      #f)

(check-expect (bytes<? #"Apple" #"apple")
	      #t)
(check-expect (bytes<? #"apple" #"Apple")
	      #f)
(check-expect (bytes<? #"a" #"b" #"c")
	      #t)


(check-expect (bytes>? #"Apple" #"apple")
	      #f)
(check-expect (bytes>? #"apple" #"Apple")
	      #t)
(check-expect (bytes>? #"c" #"b" #"a")
	      #t)

(check-expect (bytes-length #"Apple")
	      5)

(check-expect (make-bytes 5 65)
	      #"AAAAA")

(check-expect (bytes 65 112 112 108 101)
	      #"Apple")



;; example and EXAMPLES are aliases to check-expect.
(example 3 3)
(EXAMPLE 3 3)


(check-expect (foldr cons '() '(1 2 3 4))
	      '(1 2 3 4))
(check-expect (foldr (lambda (v l) (cons (add1 v) l)) '() '(1 2 3 4))
	      '(2 3 4 5))


(check-expect (foldl cons '() '(1 2 3 4))
	      '(4 3 2 1))
(check-expect (foldl + 0 '(1 2 3 4))
	      10)
(check-expect (foldl (lambda (a b result)
		       (* result (- a b)))
		     1
		     '(1 2 3)
		     '(4 5 6))
	      -27)



(check-expect (memf (lambda (arg)
		      (> arg 9))
		    '(7 8 9 10 11))
	      '(10 11))


(check-expect (build-list 10 values)
	      '(0 1 2 3 4 5 6 7 8 9))
(check-expect  (build-list 5 (lambda (x) (* x x)))
	       '(0 1 4 9 16))





(check-expect (char<? #\A #\a)
	      #t)
(check-expect (char<? #\a #\A)
	      #f)
(check-expect (char<? #\a #\b #\c)
	      #t)

(check-expect (char-ci=? #\A #\a)
	      #t)
(check-expect (char-ci=? #\a #\a #\a)
	      #t)

(check-expect (char-ci>=? #\A #\a)
	      #t)
(check-expect (char-ci>=? #\a #\A)
	      #t)
(check-expect (char-ci>=? #\c #\b #\b)
	      #t)

(check-expect (char=? #\a #\a)
	      #t)
(check-expect (char=? #\a #\A #\a)
	      #f)

(check-expect (char<=? #\A #\a)
	      #t)
(check-expect (char<=? #\a #\A)
	      #f)
(check-expect (char<=? #\a #\b #\b)
	      #t)


(check-expect (char>? #\A #\a)
	      #f)
(check-expect (char>? #\a #\A)
	      #t)
(check-expect (char>? #\c #\b #\a)
	      #t)


(check-expect (char-alphabetic? #\a) true)
(check-expect (char-alphabetic? #\0) false)

(check-expect (char-upper-case? #\a) false)
(check-expect (char-upper-case? #\A) true)
(check-expect (char-upper-case? #\0) false)

(check-expect (char-lower-case? #\a) true)
(check-expect (char-lower-case? #\A) false)
(check-expect (char-lower-case? #\0) false)


(check-expect (char-ci<? #\A #\a)
	      #f)
(check-expect (char-ci<? #\a #\b)
	      #t)
(check-expect (char-ci<? #\a #\b #\c)
	      #t)

(check-expect (char-ci>? #\A #\a)
	      #f)
(check-expect (char-ci>? #\b #\A)
	      #t)
(check-expect (char-ci>? #\c #\b #\a)
	      #t)

(check-expect (char-whitespace? #\newline) 
	      true)
(check-expect (char-whitespace? #\a)
	      false)


(check-expect (char-numeric? #\a)
	      false)
(check-expect (char-numeric? #\0)
	      true)
(check-expect (andmap char-numeric?
		      '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	      true)



(check-expect (char-upcase #\a)
	      #\A)
(check-expect (char-upcase #\space)
	      #\space)

(check-expect (char-downcase #\A)
	      #\a)
(check-expect (char-downcase #\space)
	      #\space)

(check-expect (char->integer #\A)
	      65)

(check-expect (char-ci<=? #\A #\a)
	      #t)
(check-expect (char-ci<=? #\a #\A)
	      #t)
(check-expect (char-ci<=? #\a #\b #\b)
	      #t)


(check-expect (char>=? #\A #\a)
	      #f)
(check-expect (char>=? #\a #\A)
	      #t)
(check-expect (char>=? #\c #\b #\b)
	      #t)

(check-expect (bytes->immutable-bytes (bytes 65 65 65))
	      #"AAA")
(let ([b (bytes->immutable-bytes (make-bytes 5 65))])
  (check-expect (bytes->immutable-bytes b)
		#"AAAAA")
  (check-expect (eq? (bytes->immutable-bytes b) b)
		#t))

(check-expect (subbytes #"Apple" 1 3)
	      #"pp")
(check-expect (subbytes #"Apple" 1)
	      #"pple")


(check-expect (bytes-copy  #"Apple")
  #"Apple")


(check-expect (bytes-ref #"Apple" 0)
	      65)


(let ([s (bytes 65 112 112 108 101)])
  (bytes-set! s 4 121)
  (check-expect s
		#"Apply"))


(let ([s (bytes 65 112 112 108 101)])
  (bytes-fill! s 113)
  (check-expect s
		#"qqqqq"))




(printf "Please ignore the time output (just exercising the function): ")
(time (void))




"misc.rkt end"