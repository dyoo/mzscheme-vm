#lang racket/base
(require "../location.rkt")


(define (display-location thing a-loc)
  (let* ([an-stx
          (datum->syntax #f thing (list (location-id a-loc)
                                        (location-line a-loc)
                                        (location-column a-loc)
                                        (location-offset a-loc)
                                        (location-span a-loc)))]
         [an-error
          (make-exn:fail:syntax ""
                                (current-continuation-marks)
                                (list an-stx))])
    ((error-display-handler) (format "At: line ~a" (location-line a-loc)) 
                             an-error)))
    


(provide display-location)