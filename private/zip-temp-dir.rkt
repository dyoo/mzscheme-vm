#lang s-exp "profiled-base.rkt"

(require file/zip
         racket/file
         racket/contract)

(provide/contract
 [call-with-temporary-directory->zip 
  (string? (path-string? . -> . any) . -> . (values input-port? any/c))])


;; call-with-temporary-directory->zip: string (path -> X) -> (values input-port X)
(define (call-with-temporary-directory->zip dirname with-path-f)  
  (let* ([tempdir
          (make-temporary-file "mztmp~a" 'directory #f)]
         [workdir
          (build-path tempdir dirname)])
    
    (dynamic-wind
     
     (lambda ()
       (make-directory workdir))
     
     (lambda ()
       (let ([result (with-path-f workdir)])
         (let-values ([(inp outp) (make-pipe)])
           (parameterize ([current-directory tempdir])
             (zip->output (pathlist-closure (list dirname)) outp)
             (close-output-port outp)
             (values inp result)))))
     
     (lambda ()
       (delete-directory/files tempdir)))))