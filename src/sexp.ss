#lang scheme/base
(require scheme/string
         scheme/list
         scheme/contract)

(provide/contract [sexp->js (any/c . -> . string?)])


(define LIST-CONSTRUCTOR "plt.Kernel.list")
(define VECTOR-CONSTRUCTOR "plt.Kernel.vector")
(define SYMBOL-CONSTRUCTOR "plt.types.Symbol.makeInstance")
(define FLOAT-CONSTRUCTOR "plt.types.FloatPoint.makeInstance")
(define RATIONAL-CONSTRUCTOR "plt.types.Rational.makeInstance")
(define COMPLEX-CONSTRUCTOR "plt.types.Complex.makeInstance")
(define CHARACTER-CONSTRUCTOR "plt.types.Char.makeInstance")

(define TRUE "plt.types.Logic.TRUE")
(define FALSE "plt.types.Logic.FALSE")


;; sexp->js: any -> string
(define (sexp->js expr)
  (cond
    ;; Empty
    [(empty? expr)
     "plt.types.Empty.EMPTY"]
    
    ;; Nonempty lists
    [(pair? expr)
     (let ([translations (sexps->js expr)])
       (string-append LIST-CONSTRUCTOR "(["
                      (string-join translations ",")
                      "])"))]
    ;; Vectors
    [(vector? expr)
     (let ([translations (sexps->js (vector->list expr))])
       (string-append VECTOR-CONSTRUCTOR "(["
                      (string-join translations ",")
                          "])"))]
    
    ;; Symbols
    [(symbol? expr)
     (string-append SYMBOL-CONSTRUCTOR "("
                    (string->js (symbol->string expr))
                    ")")]
    
    ;; Numbers
    [(number? expr)
     (number->js expr)]
   
    ;; Strings
    [(string? expr)
     (string->js expr)]
    
    ;; Characters
    [(char? expr)
     (character->js expr)]
    
    ;; Booleans
    [(boolean? expr)
     (boolean->js expr)]
    
    [else
     (error 'sexp->js (format "Can't translate ~s" expr))]))


;; sexps->js: (listof expr) -> (listof string)
;; Produces the quotation of a list of expressions.
(define (sexps->js exprs)
  (foldl (lambda (an-expr acc)
           (cons (sexp->js an-expr) acc))
         empty
         (reverse exprs)))


;; boolean->js: boolean -> string
(define (boolean->js a-bool)
  (cond
    [a-bool TRUE]
    [else FALSE]))



;; floating-number->js: number -> string
(define (floating-number->js a-num)
  (string-append FLOAT-CONSTRUCTOR"("
                 (cond
                   [(eqv? a-num +inf.0)
                    "Number.POSITIVE_INFINITY"]
                   [(eqv? a-num -inf.0)
                    "Number.NEGATIVE_INFINITY"]
                   [(eqv? a-num +nan.0)
                    "Number.NaN"]
                   [else
                    (number->string a-num)])
                 ")"))

;; rational-number->js: number -> string
(define (rational-number->js a-num)
  (cond [(= (denominator a-num) 1)
         (string-append RATIONAL-CONSTRUCTOR "("
                        (number->string (numerator a-num))
                        ")")]
        [else
         (string-append RATIONAL-CONSTRUCTOR "("
                        (number->string (numerator a-num))
                        ", "
                        (number->string (denominator a-num))
                        ")")]))


;; number->java-string: number -> string
(define (number->js a-num)
  (cond 
    [(rational? a-num)
     (rational-number->js a-num)]
    
    [(real? a-num)
     (floating-number->js a-num)]
    
    [(complex? a-num)
     (string-append COMPLEX-CONSTRUCTOR "("
                    (number->js (real-part a-num))
                    ", "
                    (number->js (imag-part a-num))
                    ")")]))



;; char->javascript-string: char -> string
(define (character->js a-char)
  (string-append CHARACTER-CONSTRUCTOR "("
                 "String.fromCharCode("
                 (number->string (char->integer a-char))
                 "))"))


;; excape-char-code: char -> string
(define (escape-char-code a-char)
  (cond
    [(char=? a-char #\")
     (string #\\ #\")]
    [(char=? a-char #\\)
     (string #\\ #\\)]
    [(char=? a-char #\newline)
     (string #\\ #\n)]
    [else
     (string a-char)]))
  

;; string->javascript-string: string -> string
(define (string->js a-str)
  ;; FIXME: escape all character codes!
  (string-append "\""
                 (string-join (map escape-char-code (string->list a-str))
                              "")
                 "\""))