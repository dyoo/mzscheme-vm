#lang s-exp "profiled-base.rkt"
(require racket/string
         racket/list
         racket/contract
         racket/match
         "jsexp.rkt")

(provide/contract [jsexp->js (jsexp? . -> . string?)]
                  [print-jsexp->js (jsexp? output-port? . -> . any)]

                  [sexp->js (any/c . -> . string?)]
                  [print-sexp->js (any/c output-port? . -> . any)])


(define LIST-CONSTRUCTOR "types.list")
(define PAIR-CONSTRUCTOR "types.pair")
(define VECTOR-CONSTRUCTOR "types.vectorImmutable")
(define SYMBOL-CONSTRUCTOR "types.symbol")
(define KEYWORD-CONSTRUCTOR "types.keyword")

(define FLOAT-CONSTRUCTOR "types.float")
(define NEGATIVE-ZERO "jsnums.negative_zero")
(define POSITIVE-INFINITY "jsnums.inf")
(define NEGATIVE-INFINITY "jsnums.negative_inf")
(define NOT-A-NUMBER "jsnums.nan")

(define RATIONAL-CONSTRUCTOR "types.rational")
(define BIGNUM-CONSTRUCTOR "types.bignum")
(define COMPLEX-CONSTRUCTOR "types.complex")
(define CHARACTER-CONSTRUCTOR "types.char")
(define PATH-CONSTRUCTOR "types.path")
(define BOX-CONSTRUCTOR "types.boxImmutable")
(define REGEXP-CONSTRUCTOR "types.regexp")
(define BYTE-REGEXP-CONSTRUCTOR "types.byteRegexp")
(define BYTES-CONSTRUCTOR "types.bytesImmutable")
(define HASH-CONSTRUCTOR "types.hash")
(define HASHEQ-CONSTRUCTOR "types.hasheq")

(define EMPTY "types.EMPTY")
(define TRUE "true")
(define FALSE "false")
(define VOID "types.VOID")


;; jsexp->js: jsexp -> string
(define (jsexp->js a-jsexp)
  (let ([str-buf (open-output-string)])
    (print-jsexp->js a-jsexp str-buf)
    (get-output-string str-buf)))

;; -jsexp->js: jsexp output-port -> void
(define (print-jsexp->js a-jsexp out)
  (match a-jsexp
    [(struct ht (name pairs))
     (display "{" out)
     (for-each-print/comma key-value->js
                           (cons `($ ,(make-lit (symbol->string name)))
                                            pairs)
                           out)
     (display "}" out)]
    [(struct vec (items))
     (display "[" out)
     (for-each-print/comma print-jsexp->js items out)
     (display "]" out)]
    [(struct int (v))
     (display (number->string v) out)]
    [(struct lit (v))
     (print-sexp->js v out)]))
  
;; for-each-print/comma: (X output-port -> void) (listof X) -> void
(define (for-each-print/comma f items out)
  (let ([last-index (sub1 (length items))])
    (for ([item items]
          [index (in-naturals)])
      (f item out)
      (unless (= index last-index)
        (display "," out)))))


;; key-value->js: (list symbol jsval) output-port -> void
(define (key-value->js a-key-value out)
  (let ([key (first a-key-value)]
        [value (second a-key-value)])
    (print-sexp->js (symbol->string key) out)
    (display ":" out)
    (print-jsexp->js value out)))


;; sexp->js: any -> string
(define (sexp->js expr)
  (let ([str-buf (open-output-string)])
    (print-sexp->js expr str-buf)
    (get-output-string str-buf)))

      

;; print-sexp->js: any output-port -> void
(define (print-sexp->js expr out)
  (cond
    [(void? expr)
     (display VOID out)
     VOID]

    ;; Empty
    [(empty? expr)
     (display EMPTY out)]
    
    ;; Nonempty lists
    [(list? expr)
     (display LIST-CONSTRUCTOR out)
     (display "([" out)
     (for-each-print/comma print-sexp->js expr out)
     (display "])" out)]

    ;; Dotted pairs
    [(pair? expr)
     (display PAIR-CONSTRUCTOR out)
     (display "(" out)
     (print-sexp->js (car expr) out)
     (display "," out)
     (print-sexp->js (cdr expr) out)
     (display ")" out)]
    
    ;; Vectors
    [(vector? expr)
     (display VECTOR-CONSTRUCTOR out)
     (display "([" out)
     (for-each-print/comma print-sexp->js (vector->list expr) out)
     (display "])" out)]
    
    ;; Symbols
    [(symbol? expr)
     (display SYMBOL-CONSTRUCTOR out)
     (display "(" out)
     (display (string->js (symbol->string expr)) out)
     (display ")" out)]

    ;; Keywords
    [(keyword? expr)
     (display KEYWORD-CONSTRUCTOR out)
     (display "(" out)
     (display (string->js (keyword->string expr)) out)
     (display ")" out)]
    
    ;; Numbers
    [(number? expr)
     (display (number->js expr) out)]
   
    ;; Strings
    [(string? expr)
     (display (string->js expr) out)]
    
    ;; Bytes
    [(bytes? expr)
     (display BYTES-CONSTRUCTOR out)
     (display "([" out)
     (for-each-print/comma (lambda (n out)
                             (display (number->string n) out))
                           (bytes->list expr)
                           out)
     (display "])" out)]

    ;; Characters
    [(char? expr)
     (display (character->js expr) out)]
    
    ;; Booleans
    [(boolean? expr)
     (display (boolean->js expr) out)]
 
    ;; Paths
    [(path? expr)
     (display PATH-CONSTRUCTOR out)
     (display "(" out)
     (display (string->js (path->string expr)) out)
     (display ")" out)]

    ;; Boxes
    [(box? expr)
     (display BOX-CONSTRUCTOR out)
     (display "(" out)
     (print-sexp->js (unbox expr) out)
     (display ")" out)]
    
    ;; Regexps
    [(regexp? expr)
     (display REGEXP-CONSTRUCTOR out)
     (display "(" out)
     (print-sexp->js (object-name expr) out)
     (display ")" out)]

    ;; Byte regexps
    [(byte-regexp? expr)
     (display BYTE-REGEXP-CONSTRUCTOR out)
     (display "(" out)
     (print-sexp->js (object-name expr) out)
     (display ")" out)]

    ;; hashes
    [(hash-eq? expr)
     (display HASHEQ-CONSTRUCTOR out)
     (display "(" out)
     (print-sexp->js (hash-map expr (lambda (k v)
                                               (cons k v)))
                     out)
     (display ")" out)]
    [(hash? expr)
     (display HASH-CONSTRUCTOR out)
     (display "(" out)
     (print-sexp->js (hash-map expr (lambda (k v)
                                (cons k v)))
                     out)
     (display ")" out)]

    [else
     (error 'sexp->js (format "Can't translate ~s" expr))]))




;; boolean->js: boolean -> string
(define (boolean->js a-bool)
  (cond
    [a-bool TRUE]
    [else FALSE]))



;; floating-number->js: number -> string
(define (floating-number->js a-num)
  (cond
   [(eqv? a-num -0.0)
    NEGATIVE-ZERO]
   [(eqv? a-num +inf.0)
    POSITIVE-INFINITY]
   [(eqv? a-num -inf.0)
    NEGATIVE-INFINITY]
   [(eqv? a-num +nan.0)
    NOT-A-NUMBER]
   [else
    (string-append FLOAT-CONSTRUCTOR"("
		   (number->string a-num)
		   ")")]))

;; rational-number->js: number -> string
(define (rational-number->js a-num)
  (cond [(= (denominator a-num) 1)
         (string-append (integer->js (numerator a-num)))]
        [else
         (string-append RATIONAL-CONSTRUCTOR "("
                        (integer->js (numerator a-num))
                        ", "
                        (integer->js (denominator a-num))
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
    [(and (exact? a-num) (rational? a-num))
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
  (case (char->integer a-char)
    [(0) "\\0"]
    [(7) "\\a"]
    [(8) "\\b"]
    [(9) "\\t"]
    [(10) "\\n"]
    [(11) "\\v"]
    [(12) "\\f"]
    [(13) "\\r"]
    [(32) " "]
    [(34) "\\\""]
    [(92) "\\\\"]
    [else
     (cond
       [(char-graphic? a-char)
        (string a-char)]
       [else
        (string-append "\\u"
                       (pad0 (number->string (char->integer a-char) 16) 
                             4))])]))

;; pad0: string number -> string
;; Adds the padding character #\0 in front of str so that it has the desired length.
(define (pad0 str len)
  (cond [(>= (string-length str) len)
         str]
        [else
         (string-append (build-string (- len (string-length str))(lambda (i) #\0))
                        str)]))


;; string->javascript-string: string -> string
(define (string->js a-str)
  ;; FIXME: escape all character codes!
  (string-append "\""
                 (string-join (map escape-char-code (string->list a-str))
                              "")
                 "\""))