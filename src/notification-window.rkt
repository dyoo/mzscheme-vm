#lang racket/base
(require racket/gui/base
         racket/class)


;; make-notification-window: #:title string -> output-port
(define (make-notification-window #:title title)
  (parameterize ([current-eventspace (make-eventspace)])
    (define f (new frame% [label title]))
    (define t (new text%))
    (define c (new editor-canvas%
                   [parent f]
                   [editor t]))
    (send f show #t)
    (send t lock #t)
    (let-values ([(ip op) (make-pipe)])
      (thread (lambda ()
                (let loop ()
                  (let ([ch (read-char ip)])
                    (cond [(eof-object? ch)
                           (void)]
                          [else
                           (queue-callback (lambda () 
                                             (send t lock #f)
                                             (send t insert (string ch))
                                             (send t lock #t)))
                           (loop)])))))
      op)))