#lang s-exp "profiled-base.rkt"


(provide current-log-port log-warning log-info)

(define current-log-port (make-parameter (current-output-port)))

(define (log-warning a-warning)
  (display a-warning (current-log-port)))

(define (log-info an-info)
  (display an-info (current-log-port)))