#lang s-exp "profiled-base.rkt"
(require racket/gui/base
         racket/class
         racket/contract)

;; Creates a window that we can write to.
(provide/contract [make-notification-window
                   (#:title string? . -> . output-port?)])

;; make-notification-window: #:title string -> output-port
(define (make-notification-window #:title title)
  (parameterize ([current-eventspace (make-eventspace)])
    (define f (new frame% [label title]
                   [width 500]
                   [height 300]))
    (define t (new text%))
    (define c (new editor-canvas%
                   [parent f]
                   [editor t]))
    (new button% 
         [label "Close"] 
         [parent f]
         [callback (lambda (b c) 
                     (send f show #f))]) 
    (send f show #t)
    (send t lock #t)
    (let-values ([(ip op) (make-pipe)])
      (thread (lambda ()
                (let loop ()
                  (let ([line (read-line ip)])
                    (cond [(eof-object? line)
                           (void)]
                          [else
                           (queue-callback (lambda ()
                                             (send t lock #f)
                                             (send t insert line)
                                             (send t insert "\n")
                                             (send t lock #t)))
                           (loop)])))))
      op)))