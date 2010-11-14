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




"misc.rkt end"