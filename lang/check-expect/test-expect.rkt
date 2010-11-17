#lang s-exp "../base.rkt"

(require (for-syntax racket/base)
         "../location.rkt"
         "display-location.rkt")

(provide test-expect 
         test-within 
         test-error)



(define-for-syntax (syntax-location-values stx)
  (list (syntax-source stx) ;; can be path or symbol
        (syntax-position stx)
        (syntax-line stx)
        (syntax-column stx)
        (syntax-span stx)))


(define-for-syntax (check-at-toplevel! who stx)
  (unless (eq? (syntax-local-context) 'module)
    (raise-syntax-error #f 
                        (format "~a: found a test that is not at the top level."
                                who)
                        stx)))


(define-syntax (test-expect stx)
  (syntax-case stx ()
    [(_ test expected)
     (begin
       (check-at-toplevel! 'test-expect stx)
       (with-syntax ([stx stx]
                     [(id offset line column span)
                      (syntax-location-values stx)])
         #'(accumulate-test!
            (lambda ()
              (test-expect* 'stx
                             (make-location id offset line column span)
                             (lambda () test)
                             (lambda () expected))))))]))
    
(define-syntax (test-within stx)
  (syntax-case stx ()
    [(_ test expected delta)
     (begin
       (check-at-toplevel! 'test-within stx)
       (with-syntax ([stx stx]
                     [(id offset line column span)
                      (syntax-location-values stx)])
         #'(accumulate-test!
            (lambda ()
              (test-within* 'stx
                             (make-location id offset line column span)
                             (lambda () test)
                             (lambda () expected)
                             (lambda () delta))))))]))

(define-syntax (test-error stx)
  (syntax-case stx ()
    [(_ test expected-msg)
     (begin
       (check-at-toplevel! 'test-error stx)
       (with-syntax ([stx stx]
                     [(id offset line column span)
                      (syntax-location-values stx)])
         #'(accumulate-test!
            (lambda ()
              (test-error* 'stx
                            (make-location id offset line column span)
                            (lambda () test)
                            (lambda () expected-msg))))))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-expect* test-datum a-loc test-thunk expected-thunk)
  (with-handlers ([void
                   (lambda (exn)
                     (printf "test-expect: ~s"
                             (exn-message exn))
                     (newline)
                     (display-location test-datum a-loc)
                     #f)])
    (let ([expected-value (expected-thunk)]
          [test-value (test-thunk)])
      (cond
        [(equal? test-value expected-value)
         #t]
        [else
         (printf "test-expect: actual value ~s differs from ~s, the expected value" test-value expected-value)
         (newline)
         (display-location test-datum a-loc)
         #f]))))


(define (test-within* test-datum a-loc test-thunk expected-thunk delta-thunk)
  (with-handlers ([void
                   (lambda (exn)
                     (printf "test-within: ~s"
                             (exn-message exn))
                     (newline)
                     (display-location test-datum a-loc)
                     #f)])
    (with-handlers ([void
                     (lambda (exn)
                       (printf "test-within: ~s"
                               (exn-message exn))
                       (newline)
                       (display-location test-datum a-loc)
                       #f)])
      (let ([expected-value (expected-thunk)]
            [test-value (test-thunk)]
            [delta-value (delta-thunk)])
        (cond
          [(not (real? delta-value))
           (printf "test-within requires an inexact number for the range.  ~s is not inexact.\n" delta-value)
           (display-location test-datum a-loc)
           #f]
          [(equal~? test-value expected-value delta-value)
           #t]
          [else
           (printf "test-within: actual value ~s differs from ~s, the expected value.\n" test-value expected-value)
           (display-location test-datum a-loc)
           #f])))))



(define (test-error* test-datum a-loc test-thunk expected-message-thunk)
  (with-handlers ([void
                   (lambda (exn)
                     (printf "test-error: ~s"
                             (exn-message exn))
                     (newline)
                     (display-location test-datum a-loc)
                     #f)])
    (let ([expected-message (expected-message-thunk)])
      (with-handlers 
          ([unexpected-no-error?
            (lambda (une)
              (printf "test-error expected the error ~s, but got ~s instead.\n"
                      expected-message
                      (unexpected-no-error-result une))
              (display-location test-datum a-loc)
              #f)]
           [exn:fail?
            (lambda (exn)
              (cond [(string=? (exn-message exn) expected-message)
                     #t]
                    [else
                     (printf "test-error: expected the error ~s, but got ~s instead.\n"
                             expected-message
                             (exn-message exn))
                     (display-location test-datum a-loc)
                     #f]))])
        (let ([result (test-thunk)])
          (raise (make-unexpected-no-error result)))))))
  






;; a test is a thunk of type: (-> boolean)
;; where it returns true if the test was successful,
;; false otherwise.

;; accumulate-test!
(define (accumulate-test! a-test)
  (set! *tests* (cons a-test *tests*)))

    
;; test-suffixed: number -> string
(define (test-suffixed n)
  (case n 
    [(0) "zero tests"]
    [(1) "one test"]
    [else (format "~a tests" n)]))
  

;; capitalize: string -> string
(define (capitalize s)
  (cond [(> (string-length s) 0)
         (string-append (string (char-upcase (string-ref s 0)))
                        (substring s 1))]
        [else
         s]))


;; run-tests: -> void
(define (run-tests)
  (when (> (length *tests*) 0)
    ;; Run through the tests
    (let loop ([tests-passed 0]
               [tests-failed 0]
               [tests (reverse *tests*)])
      (cond
        [(empty? tests)
         ;; Report test results
         (cond [(= tests-passed (length *tests*))
                (display (case (length *tests*)
                           [(1) "The test passed!"]
                           [(2) "Both tests passed!"]
                           [else
                            (format "All ~a tests passed!"
                                    (length *tests*))]))
                (newline)]
               [else
                (printf "Ran ~a.\n" 
                        (test-suffixed (length *tests*)))
                (printf "~a passed.\n" 
                        (capitalize (test-suffixed tests-passed)))
                (printf "~a failed.\n" 
                        (capitalize (test-suffixed tests-failed)))])
         (set! *tests* '())]
        [else
         (let* ([test-thunk (first tests)]
                [test-result (test-thunk)])
           (cond
             [test-result
              (loop (add1 tests-passed)
                    tests-failed
                    (rest tests))]
             [else
              (loop tests-passed
                    (add1 tests-failed)
                    (rest tests))]))]))))
  


(define-struct unexpected-no-error (result))
