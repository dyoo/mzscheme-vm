#lang racket/base

(require racket/runtime-path
         racket/contract
         racket/port)

(define-runtime-path library-path "../lib")

;; cat-to-port: path output-port -> void
;; Write out contents of path to output port.
(define (cat-to-port a-path out-port)
  (call-with-input-file a-path (lambda (ip) (copy-port ip out-port))))


;; write-platform-libraries: string output-port -> void
;; Writes out the platform-specific libraries out to the given output port.
(define (write-support a-platform out-port)
  (let ([platform-specific-js-path
         (build-path library-path (string-append a-platform "-platform.js"))])
    
    (cond
      [(file-exists? platform-specific-js-path)
       (cat-to-port platform-specific-js-path out-port)
       (call-with-input-file (build-path library-path "order")
         (lambda (order-ip)
           (for ([filename (in-lines order-ip)])
             (cat-to-port (build-path library-path filename) out-port))))]
      [else
       (error 'mobyc (format "No support for platform ~s" a-platform))])))


(provide/contract [write-support (string? output-port? . -> . any)])