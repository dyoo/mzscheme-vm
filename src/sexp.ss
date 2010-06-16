#lang scheme/base
(require scheme/string
         scheme/list
         scheme/contract
         scheme/match
         "jsexp.ss")

(provide/contract [jsexp->js (jsexp? . -> . string?)]
                  [sexp->js (any/c . -> . string?)])


(define LIST-CONSTRUCTOR "_runtime.list")
(define PAIR-CONSTRUCTOR "_runtime.pair")
(define VECTOR-CONSTRUCTOR "_runtime.vector")
(define SYMBOL-CONSTRUCTOR "_runtime.symbol")
(define KEYWORD-CONSTRUCTOR "_runtime.keyword")
(define FLOAT-CONSTRUCTOR "_runtime.float")
(define RATIONAL-CONSTRUCTOR "_runtime.rational")
(define BIGNUM-CONSTRUCTOR "_runtime.bignum")
(define COMPLEX-CONSTRUCTOR "_runtime.complex")
(define CHARACTER-CONSTRUCTOR "_runtime.char")
(define PATH-CONSTRUCTOR "_runtime.path")
(define BOX-CONSTRUCTOR "_runtime.box")
(define REGEXP-CONSTRUCTOR "_runtime.regexp")
(define BYTE-REGEXP-CONSTRUCTOR "_runtime.byteRegexp")
(define BYTES-CONSTRUCTOR "_runtime.bytes")


(define EMPTY "_runtime.EMPTY")
(define TRUE "true")
(define FALSE "false")
(define VOID "_runtime.VOID")


;; jsexp->js: jsexp -> string
(define (jsexp->js a-jsexp)
  (match a-jsexp
    [(struct ht (name pairs))
     (string-append "{"
                    (string-join (map key-value->js 
                                      (cons `($ ,(make-lit (symbol->string name)))
                                            pairs))
                                 ",")
                    "\n"
                    "}")]
    [(struct vec (items))
     (string-append "[" 
                    (string-join (map jsexp->js items) ",")
                    "\n"
                    "]")]
    [(struct int (v))
     (number->string v)]
    [(struct lit (v))
     (sexp->js v)]))
  

;; key-value->js: (list symbol jsval) -> string
(define (key-value->js a-key-value)
  (let ([key (first a-key-value)]
        [value (second a-key-value)])
    (string-append (sexp->js (symbol->string key))
                   ":"
                   (jsexp->js value))))

      


;; sexp->js: any -> string
(define (sexp->js expr)
  (cond
    [(void? expr)
     VOID]
    
    ;; Empty
    [(empty? expr)
     EMPTY]
    
    ;; Nonempty lists
    [(list? expr)
     (let ([translations (sexps->js expr)])
       (string-append LIST-CONSTRUCTOR "(["
                      (string-join translations ",")
                      "])"))]

    ;; Dotted pairs
    [(pair? expr)
     (string-append PAIR-CONSTRUCTOR "("
                    (sexp->js (car expr))
                    ","
                    (sexp->js (cdr expr))
                    ")")]
    
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

    ;; Keywords
    [(keyword? expr)
     (string-append KEYWORD-CONSTRUCTOR "("
                    (string->js (keyword->string expr))
                    ")")]
    
    ;; Numbers
    [(number? expr)
     (number->js expr)]
   
    ;; Strings
    [(string? expr)
     (string->js expr)]
    
    ;; Bytes
    [(bytes? expr)
     (string-append BYTES-CONSTRUCTOR "("
                    (string-join (map number->string (bytes->list expr)) ",")
                    ")")]

    ;; Characters
    [(char? expr)
     (character->js expr)]
    
    ;; Booleans
    [(boolean? expr)
     (boolean->js expr)]
 
    ;; Paths
    [(path? expr)
     (string-append PATH-CONSTRUCTOR "(" 
                    (string->js (path->string expr))
                    ")")]

    ;; Boxes
    [(box? expr)
     (string-append BOX-CONSTRUCTOR "("
                    (sexp->js (unbox expr))
                    ")")]
    
    ;; Regexps
    [(regexp? expr)
     (string-append REGEXP-CONSTRUCTOR "("
                    (sexp->js (object-name expr))
                    ")")]

    ;; Byte regexps
    [(byte-regexp? expr)
     (string-append BYTE-REGEXP-CONSTRUCTOR "("
                    (sexp->js (object-name expr))
                    ")")]

    
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

;; integer->js: int -> string
(define (integer->js an-int)
  (cond
    ;; non-overflow case
    [(< (abs an-int) 9e15)
     (number->string an-int)]
    ;; overflow case
    [else
     (string-append BIGNUM-CONSTRUCTOR 
                    "("
                    (string->js (number->string an-int))
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