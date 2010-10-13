#lang racket/base


(provide current-log-port log-warning)

(define current-log-port (make-parameter (current-output-port)))

(define (log-warning a-warning)
  (display a-warning (current-log-port)))