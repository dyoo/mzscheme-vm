#lang racket/base

(require racket/runtime-path
         racket/contract
         racket/port)

(define-runtime-path library-path "../lib")
(define-runtime-path support-directory "../support")


;; cat-to-port: path output-port -> void
;; Write out contents of path to output port.
(define (cat-to-port a-path out-port)
  (call-with-input-file a-path (lambda (ip) (copy-port ip out-port))))


;; copy-support-files: path -> void
;; Write out the support files to the given directory.
(define (copy-support-files a-path)
  (for ([p (directory-list support-directory)])
    (when (file-exists? (build-path a-path p))
      (delete-file (build-path a-path p)))
    
    (copy-file (build-path support-directory p)
               (build-path a-path p))))


;; write-platform-libraries: string output-port -> void
;; Writes out the platform-specific libraries out to the given output port.
(define (write-runtime a-platform out-port)
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
       (error 'mobyc (format "No runtime for platform ~s" a-platform))])))





(provide/contract [write-runtime (string? output-port? . -> . any)]
                  [copy-support-files (path? . -> . any)])