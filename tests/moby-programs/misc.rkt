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






"misc.rkt end"