#lang s-exp "../base.rkt"

(require (for-syntax racket/base)
         "../location.rkt")

(provide check-expect 
         check-within 
         check-error
         run-tests)

(define *tests* '())


(define-for-syntax (syntax-location-values stx)
  (list (format "~a" (syntax-source stx))
        (syntax-position stx)
        (syntax-line stx)
        (syntax-column stx)
        (syntax-span stx)))


(define-syntax (check-expect stx)
  (syntax-case stx ()
    [(_ test expected)
     (with-syntax ([(id offset line column span)
                    (syntax-location-values stx)])
       #'(accumulate-test!
          (lambda ()
            (check-expect* (make-location id offset line column span)
                           (lambda () test)
                           (lambda () expected)))))]))
    
(define-syntax (check-within stx)
  (syntax-case stx ()
    [(_ test expected delta)
     #'(void)]))


(define-syntax (check-error stx)
  (syntax-case stx ()
    [(_ test expected-msg)
     #'(void)]))


(define (check-expect* a-loc test-thunk expected-thunk)
  ;; add exception handlers
  (with-handlers ([void
                   (lambda (exn)
                     (printf "check-expect: ~s"
                             (exn-message exn))
                     (newline)
                     (display a-loc)
                     #f)])
    (let ([expected-value (expected-thunk)]
          [test-value (test-thunk)])
      (cond
        [(equal? test-value expected-value)
         #t]
        [else
         (printf "check-expect: actual value ~s differs from ~s, the expected value" test-value expected-value)
         (newline)
         (display-location a-loc)
         #f]))))


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
  

;; run-tests: -> void
(define (run-tests)
  (when (> (length *tests*) 0)
    ;; Run through the tests
    (let loop ([tests-passed 0]
               [tests-failed 0]
               [tests *tests*])
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
                (printf "~a passed.\n" tests-passed)
                (printf "~a failed.\n" tests-failed)])
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
  
  
  
  
  
  #|
(define-struct unexpected-no-error (result))

(define (error-matches? exn msg)
  (string=? (exn-message exn) msg))


(define (check-error/thunk error-thunk expected-message)
  (with-handlers 
      ([unexpected-no-error?
        (lambda (une)
          (error 
           'check-error
           (format
            "check-error expected the error ~s, but got ~s instead."
            expected-message
            (unexpected-no-error-result une))))]
       [exn:fail?
        (lambda (exn)
          (unless (error-matches? exn expected-message)
            (error 'check-error
                   (format 
                    "check-error expected the error ~s, but got ~s instead."
                    expected-message
                    (exn-message exn)))))])
    (let ([result (error-thunk)])
      (raise (make-unexpected-no-error result)))))


(define-syntax (check-error stx)
  (syntax-case stx ()
    [(_ expr str)
     (syntax/loc stx
       (check-error/thunk (lambda () expr)
                          str))]))



|#
