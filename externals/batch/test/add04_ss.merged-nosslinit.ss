(module add04_ss.merged '#%kernel
  (define-values
   (lift11422)
   (lambda (arg0-11423 arg1-11424)
     '#(...e/more-scheme.ss:220:7
        "/Users/jay/Dev/svn/plt/collects/scheme/private/more-scheme.ss"
        220
        7
        6847
        727
        #t)
     '(captures: #%modvars)
     (with-continuation-mark
      |_break-enabled-key@(quote #%paramz)|
      arg0-11423
      (with-continuation-mark
       |_exception-handler-key@(quote #%paramz)|
       lift11421
       (arg1-11424)))))
  (define-values
   (lift11421)
   (lambda (arg0-11425)
     '#(...e/more-scheme.ss:232:15
        "/Users/jay/Dev/svn/plt/collects/scheme/private/more-scheme.ss"
        232
        15
        7365
        180
        #t)
     '(captures: #%modvars)
     (abort-current-continuation _handler-prompt-key11321 arg0-11425)))
  (define-values
   (lift11420)
   (lambda (arg0-11428 arg1-11429 arg2-11430)
     '#(loop
        "/Users/jay/Dev/svn/plt/collects/scheme/private/more-scheme.ss"
        175
        6
        5620
        302
        #f)
     '(captures: #%modvars)
     (if (null? arg2-11430)
       (raise arg0-11428)
       (if ((caar arg2-11430) arg0-11428)
         (begin0
           ((cdar arg2-11430) arg0-11428)
           (with-continuation-mark
            |_break-enabled-key@(quote #%paramz)|
            arg1-11429
            (|_check-for-break@(quote #%paramz)|)))
         (lift11420 arg0-11428 arg1-11429 (cdr arg2-11430))))))
  (define-values
   (_select-handler/no-breaks11319)
   (lambda (arg0-11441 arg1-11442 arg2-11443)
     '#(select-handler/no-breaks
        "/Users/jay/Dev/svn/plt/collects/scheme/private/more-scheme.ss"
        170
        2
        5413
        511
        #f)
     '(captures: #%modvars)
     (with-continuation-mark
      |_break-enabled-key@(quote #%paramz)|
      (make-thread-cell '#f)
      (lift11420 arg0-11441 arg1-11442 arg2-11443))))
  (define-values (_false-thread-cell11320) (make-thread-cell '#f))
  (define-values (_handler-prompt-key11321) (make-continuation-prompt-tag))
  (define-values
   (_call-handled-body11322)
   (lambda (arg0-11449 arg1-11450 arg2-11451)
     '#(call-handled-body
        "/Users/jay/Dev/svn/plt/collects/scheme/private/more-scheme.ss"
        213
        2
        6537
        1107
        #f)
     '(captures: #%modvars)
     (with-continuation-mark
      |_break-enabled-key@(quote #%paramz)|
      _false-thread-cell11320
      (call-with-continuation-prompt
       lift11422
       _handler-prompt-key11321
       arg1-11450
       arg0-11449
       arg2-11451))))
  (define-values
   (_struct:keyword-procedure11323
    _make-keyword-procedure11324
    _keyword-procedure?11325
    _keyword-procedure-proc11326
    _set-keyword-procedure-proc!11327
    _keyword-procedure-required11328
    _set-keyword-procedure-required!11329
    _keyword-procedure-allowed11330
    _set-keyword-procedure-allowed!11331)
   (let ((local11457 (current-inspector)))
     (begin
       (if local11457
         (if (not (inspector? local11457))
           (raise-type-error 'define-struct '"inspector or #f" local11457)
           (void))
         (void))
       (let ((localv11463 ?)
             (localv11464 ?)
             (localv11465 ?)
             (localv11466 ?)
             (localv11467 ?))
         (begin
           (set!-values (localv11463
                         localv11464
                         localv11465
                         localv11466
                         localv11467)
             (make-struct-type
              'keyword-procedure
              '#f
              '3
              '0
              '#f
              '()
              local11457))
           (values
            localv11463
            localv11464
            localv11465
            (make-struct-field-accessor localv11466 '0 'proc)
            (make-struct-field-mutator localv11467 '0 'proc)
            (make-struct-field-accessor localv11466 '1 'required)
            (make-struct-field-mutator localv11467 '1 'required)
            (make-struct-field-accessor localv11466 '2 'allowed)
            (make-struct-field-mutator localv11467 '2 'allowed)))))))
  (define-values
   (_struct:keyword-method11332
    _make-km11333
    _keyword-method?11334
    _km-ref11335
    _km-set!11336)
   (make-struct-type 'procedure _struct:keyword-procedure11323 '0 '0 '#f))
  (define-values
   (_generate-arity-string11337)
   (lambda (arg0-11507)
     '#(generate-arity-string
        "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
        31
        2
        1001
        2388
        #f)
     '(captures: #%modvars)
     (let ((localv11508 ?) (localv11509 ?) (localv11510 ?) (localv11511 ?))
       (begin
         (set!-values (localv11508 localv11509)
           (_procedure-keywords11338 arg0-11507))
         (begin
           (set!-values (localv11510) (procedure-arity arg0-11507))
           (begin
             (set!-values (localv11511)
               (lambda (arg0-11514 arg1-11515)
                 '#(keywords-desc
                    "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
                    35
                    18
                    1195
                    890
                    #f)
                 (format
                  '"~a with keyword~a~a"
                  (if (null? (cdr arg1-11515))
                    (format '"an ~aargument" arg0-11514)
                    (format '"~aarguments" arg0-11514))
                  (if (null? (cdr arg1-11515)) '"" '"s")
                  (let ((local11528 (length arg1-11515)))
                    (if (eq? local11528 '1)
                      (begin local11528 (format '" ~a" (car arg1-11515)))
                      (if (eq? local11528 '2)
                        (format
                         '" ~a and ~a"
                         (car arg1-11515)
                         (cadr arg1-11515))
                        ((lambda (arg0-11543)
                           '#(loop
                              "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
                              47
                              31
                              1810
                              271
                              #f)
                           (if (null? (cdr arg0-11543))
                             (format '" and ~a" (car arg0-11543))
                             (format
                              '" ~a,~a"
                              (car arg0-11543)
                              (let ((local11553 (cdr arg0-11543)))
                                (if (null? (cdr local11553))
                                  (format '" and ~a" (car local11553))
                                  (format
                                   '" ~a,~a"
                                   (car local11553)
                                   (let ((local11564 (cdr local11553)))
                                     (if (null? (cdr local11564))
                                       (format '" and ~a" (car local11564))
                                       (format
                                        '" ~a,~a"
                                        (car local11564)
                                        (loop922 (cdr local11564)))))))))))
                         arg1-11515)))))))
             (string-append
              (if (number? localv11510)
                (let ((local11581
                       (if (if (_okm?11339 arg0-11507)
                             (begin arg0-11507 '#t)
                             (_keyword-method?11334 arg0-11507))
                         (if (zero? localv11510)
                           (begin localv11510 '0)
                           (sub1 localv11510))
                         localv11510)))
                  (format
                   '"~a argument~a"
                   local11581
                   (if (= local11581 '1) '"" '"s")))
                (if (arity-at-least? localv11510)
                  (let ((local11592
                         (let ((local11593 (arity-at-least-value localv11510)))
                           (if (if (_okm?11339 arg0-11507)
                                 '#t
                                 (_keyword-method?11334 arg0-11507))
                             (if (zero? local11593) '0 (sub1 local11593))
                             local11593))))
                    (format
                     '"at least ~a argument~a"
                     local11592
                     (if (= local11592 '1) '"" '"s")))
                  (begin localv11510 '"a different number of arguments")))
              (if (null? localv11508)
                '""
                (format '" plus ~a" (localv11511 '"" localv11508)))
              (if localv11509
                (let ((local11609
                       ((lambda (arg0-11612 arg1-11613)
                          '#(loop
                             "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
                             73
                             25
                             2832
                             353
                             #f)
                          (if (null? arg0-11612)
                            arg1-11613
                            (if (eq? (car arg0-11612) (car arg1-11613))
                              (let ((local11619 (cdr arg0-11612)))
                                (let ((local11621 (cdr arg1-11613)))
                                  (if (null? local11619)
                                    local11621
                                    (if (eq? (car local11619) (car local11621))
                                      (let ((local11628 (cdr local11619)))
                                        (let ((local11630 (cdr local11621)))
                                          (if (null? local11628)
                                            local11630
                                            (if (eq?
                                                 (car local11628)
                                                 (car local11630))
                                              (loop923
                                               (cdr local11628)
                                               (cdr local11630))
                                              (cons
                                               (car local11630)
                                               (loop923
                                                local11628
                                                (cdr local11630)))))))
                                      (cons
                                       (car local11621)
                                       (let ((local11650 (cdr local11621)))
                                         (if (null? local11619)
                                           local11650
                                           (if (eq?
                                                (car local11619)
                                                (car local11650))
                                             (loop923
                                              (cdr local11619)
                                              (cdr local11650))
                                             (cons
                                              (car local11650)
                                              (loop923
                                               local11619
                                               (cdr local11650)))))))))))
                              (cons
                               (car arg1-11613)
                               (let ((local11670 (cdr arg1-11613)))
                                 (if (null? arg0-11612)
                                   local11670
                                   (if (eq? (car arg0-11612) (car local11670))
                                     (let ((local11677 (cdr arg0-11612)))
                                       (let ((local11679 (cdr local11670)))
                                         (if (null? local11677)
                                           local11679
                                           (if (eq?
                                                (car local11677)
                                                (car local11679))
                                             (loop923
                                              (cdr local11677)
                                              (cdr local11679))
                                             (cons
                                              (car local11679)
                                              (loop923
                                               local11677
                                               (cdr local11679)))))))
                                     (cons
                                      (car local11670)
                                      (let ((local11699 (cdr local11670)))
                                        (if (null? arg0-11612)
                                          local11699
                                          (if (eq?
                                               (car arg0-11612)
                                               (car local11699))
                                            (loop923
                                             (cdr arg0-11612)
                                             (cdr local11699))
                                            (cons
                                             (car local11699)
                                             (loop923
                                              arg0-11612
                                              (cdr local11699))))))))))))))
                        localv11508
                        localv11509)))
                  (if (null? local11609)
                    '""
                    (format
                     '" plus ~a"
                     (localv11511 '"optional " local11609))))
                '" plus arbitrary keyword arguments"))))))))
  (define-values
   (_struct:okp11340
    _make-optional-keyword-procedure11341
    _okp?11342
    _okp-ref11343
    _okp-set!11344)
   (make-struct-type
    'procedure
    _struct:keyword-procedure11323
    '1
    '0
    '#f
    (list (cons prop:arity-string _generate-arity-string11337))
    (current-inspector)
    '0))
  (define-values
   (_struct:okm11345
    _make-optional-keyword-method11346
    _okm?11339
    _okm-ref11347
    _okm-set!11348)
   (make-struct-type 'procedure _struct:okp11340 '0 '0 '#f))
  (define-values
   (_new-prop:procedure11349 _new-procedure?11350 _new-procedure-ref11351)
   (make-struct-type-property
    'procedure
    '#f
    (list (cons prop:procedure values))))
  (define-values
   (_*make-keyword-procedure11352)
   (let ((localv11743 (box ?)))
     (begin
       (set-boxes!
        (localv11743)
        (case-lambda
         ((arg0-11744)
          '#(...me/private/kw.ss:128:16
             "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
             128
             16
             5426
             158
             #t)
          '(captures: localv11743)
          ((unbox localv11743)
           arg0-11744
           (lambda rest11747
             '#(...me/private/kw.ss:130:25
                "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
                130
                25
                5514
                68
                #t)
             '(captures: arg0-11744)
             (apply arg0-11744 '() '() rest11747))))
         ((arg0-11752 arg1-11753)
          '#(...me/private/kw.ss:132:16
             "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
             132
             16
             5601
             166
             #t)
          '(captures: #%modvars)
          (_make-optional-keyword-procedure11341
           arg0-11752
           '()
           '#f
           arg1-11753))))
       (unbox localv11743))))
  (define-values
   (_keyword-apply11353)
   (lambda (arg0-11758 arg1-11759 arg2-11760 . rest11761)
     '#(keyword-apply
        "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
        140
        2
        5830
        1799
        #f)
     '(captures: #%modvars)
     (begin
       (if (procedure? arg0-11758)
         (void)
         (apply
          raise-type-error
          'keyword-apply
          '"procedure"
          '0
          arg0-11758
          arg1-11759
          arg2-11760
          rest11761))
       ((lambda (arg0-11776 arg1-11777 arg2-11778 arg3-11779 arg4-11780)
          '#(loop
             "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
             153
             6
             6251
             459
             #f)
          (if (null? arg4-11780)
            (void)
            (if (if (pair? arg4-11780) (not (keyword? (car arg4-11780))) '#t)
              (apply
               raise-type-error
               'keyword-apply
               '"list of keywords"
               '1
               arg0-11776
               arg1-11777
               arg2-11778
               arg3-11779)
              (if (null? (cdr arg4-11780))
                (void)
                (if (if (pair? (cdr arg4-11780))
                      (not (keyword? (cadr arg4-11780)))
                      '#t)
                  (loop925
                   arg0-11776
                   arg1-11777
                   arg2-11778
                   arg3-11779
                   (cdr arg4-11780))
                  (if (keyword<? (car arg4-11780) (cadr arg4-11780))
                    (loop925
                     arg0-11776
                     arg1-11777
                     arg2-11778
                     arg3-11779
                     (cdr arg4-11780))
                    (apply
                     raise-type-error
                     'keyword-apply
                     '"sorted list of keywords"
                     '1
                     arg0-11776
                     arg1-11777
                     arg2-11778
                     arg3-11779)))))))
        arg0-11758
        arg1-11759
        arg2-11760
        rest11761
        arg1-11759)
       (if (list? arg2-11760)
         (void)
         (apply
          raise-type-error
          'keyword-apply
          '"list"
          '2
          arg0-11758
          arg1-11759
          arg2-11760
          rest11761))
       (if (= (length arg1-11759) (length arg2-11760))
         (void)
         (raise-mismatch-error
          'keyword-apply
          (format
           '"keyword list: ~e; does not match the length of the value list: "
           arg1-11759)
          arg2-11760))
       (let ((local11843
              ((lambda (arg0-11850
                        arg1-11851
                        arg2-11852
                        arg3-11853
                        arg4-11854
                        arg5-11855)
                 '#(loop
                    "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
                    176
                    13
                    7039
                    363
                    #f)
                 (if (null? (cdr arg4-11854))
                   (let ((local11858 (car arg4-11854)))
                     (if (list? local11858)
                       local11858
                       (apply
                        raise-type-error
                        'keyword-apply
                        '"list"
                        arg5-11855
                        arg0-11850
                        arg1-11851
                        arg2-11852
                        arg3-11853)))
                   (cons
                    (car arg4-11854)
                    (let ((local11872 (cdr arg4-11854)))
                      (let ((local11874 (add1 arg5-11855)))
                        (if (null? (cdr local11872))
                          (let ((local11878 (car local11872)))
                            (if (list? local11878)
                              local11878
                              (begin
                                local11878
                                (apply
                                 raise-type-error
                                 'keyword-apply
                                 '"list"
                                 local11874
                                 arg0-11850
                                 arg1-11851
                                 arg2-11852
                                 arg3-11853))))
                          (cons
                           (car local11872)
                           (loop926
                            arg0-11850
                            arg1-11851
                            arg2-11852
                            arg3-11853
                            (cdr local11872)
                            (add1 local11874)))))))))
               arg0-11758
               arg1-11759
               arg2-11760
               rest11761
               rest11761
               '3)))
         (if (null? arg1-11759)
           (apply arg0-11758 local11843)
           (apply
            (let ((local11907 (+ '2 (length local11843))))
              (_keyword-procedure-extract/method11355
               arg1-11759
               local11907
               arg0-11758
               '0))
            arg1-11759
            arg2-11760
            local11843))))))
  (define-values
   (_procedure-keywords11338)
   (lambda (arg0-11915)
     '#(procedure-keywords
        "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
        192
        2
        7633
        652
        #f)
     '(captures: #%modvars)
     (if (_keyword-procedure?11325 arg0-11915)
       (values
        (_keyword-procedure-required11328 arg0-11915)
        (_keyword-procedure-allowed11330 arg0-11915))
       (if (procedure? arg0-11915)
         (let ((local11922 (procedure-extract-target arg0-11915)))
           (if local11922
             (begin
               arg0-11915
               (if (_keyword-procedure?11325 local11922)
                 (values
                  (_keyword-procedure-required11328 local11922)
                  (_keyword-procedure-allowed11330 local11922))
                 (if (procedure? local11922)
                   (let ((local11930 (procedure-extract-target local11922)))
                     (if local11930
                       (_procedure-keywords11338 local11930)
                       (begin
                         local11930
                         (if (_new-procedure?11350 local11922)
                           (let ((local11934
                                  (_new-procedure-ref11351 local11922)))
                             (if (procedure? local11934)
                               (_procedure-keywords11338 local11934)
                               (values '() '())))
                           (values '() '())))))
                   (raise-type-error
                    'procedure-keywords
                    '"procedure"
                    local11922))))
             (if (_new-procedure?11350 arg0-11915)
               (let ((local11946 (_new-procedure-ref11351 arg0-11915)))
                 (if (procedure? local11946)
                   (if (_keyword-procedure?11325 local11946)
                     (values
                      (_keyword-procedure-required11328 local11946)
                      (_keyword-procedure-allowed11330 local11946))
                     (if (procedure? local11946)
                       (let ((local11955
                              (procedure-extract-target local11946)))
                         (if local11955
                           (_procedure-keywords11338 local11955)
                           (begin
                             local11955
                             (if (_new-procedure?11350 local11946)
                               (let ((local11959
                                      (_new-procedure-ref11351 local11946)))
                                 (if (procedure? local11959)
                                   (_procedure-keywords11338 local11959)
                                   (values '() '())))
                               (values '() '())))))
                       (raise-type-error
                        'procedure-keywords
                        '"procedure"
                        local11946)))
                   (values '() '())))
               (values '() '()))))
         (raise-type-error 'procedure-keywords '"procedure" arg0-11915)))))
  (define-values
   (_check-kw-args11354)
   (lambda (arg0-11977 arg1-11978)
     '#(check-kw-args
        "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
        723
        2
        35661
        749
        #f)
     '(captures: #%modvars)
     ((lambda (arg0-11982 arg1-11983 arg2-11984)
        '#(loop
           "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
           724
           4
           35695
           714
           #f)
        (if (null? arg0-11982)
          (if (null? arg1-11983)
            (values '#f '#f)
            (values (car arg1-11983) '#f))
          (if (if (pair? arg1-11983)
                (eq? (car arg1-11983) (car arg0-11982))
                '#f)
            (let ((local11997 (cdr arg0-11982)))
              (let ((local11999 (cdr arg1-11983)))
                (let ((local12001 (if arg2-11984 (cdr arg2-11984) '#f)))
                  (if (null? local11997)
                    (if (null? local11999)
                      (values '#f '#f)
                      (values (car local11999) '#f))
                    (if (if (pair? local11999)
                          (eq? (car local11999) (car local11997))
                          '#f)
                      (loop927
                       (cdr local11997)
                       (cdr local11999)
                       (if local12001 (cdr local12001) '#f))
                      (if local12001
                        (if (pair? local12001)
                          (if (eq? (car local12001) (car local11997))
                            (loop927
                             (cdr local11997)
                             local11999
                             (cdr local12001))
                            (loop927 local11997 local11999 (cdr local12001)))
                          (values '#f (car local11997)))
                        (loop927 (cdr local11997) local11999 '#f)))))))
            (if arg2-11984
              (if (pair? arg2-11984)
                (if (eq? (car arg2-11984) (car arg0-11982))
                  (let ((local12047 (cdr arg0-11982)))
                    (let ((local12049 (cdr arg2-11984)))
                      (if (null? local12047)
                        (if (null? arg1-11983)
                          (values '#f '#f)
                          (values (car arg1-11983) '#f))
                        (if (if (pair? arg1-11983)
                              (eq? (car arg1-11983) (car local12047))
                              '#f)
                          (loop927
                           (cdr local12047)
                           (cdr arg1-11983)
                           (if local12049 (cdr local12049) '#f))
                          (if local12049
                            (if (pair? local12049)
                              (if (eq? (car local12049) (car local12047))
                                (loop927
                                 (cdr local12047)
                                 arg1-11983
                                 (cdr local12049))
                                (loop927
                                 local12047
                                 arg1-11983
                                 (cdr local12049)))
                              (values '#f (car local12047)))
                            (loop927 (cdr local12047) arg1-11983 '#f))))))
                  (let ((local12090 (cdr arg2-11984)))
                    (if (null? arg0-11982)
                      (if (null? arg1-11983)
                        (values '#f '#f)
                        (values (car arg1-11983) '#f))
                      (if (if (pair? arg1-11983)
                            (eq? (car arg1-11983) (car arg0-11982))
                            '#f)
                        (loop927
                         (cdr arg0-11982)
                         (cdr arg1-11983)
                         (if local12090 (cdr local12090) '#f))
                        (if local12090
                          (if (pair? local12090)
                            (if (eq? (car local12090) (car arg0-11982))
                              (loop927
                               (cdr arg0-11982)
                               arg1-11983
                               (cdr local12090))
                              (loop927 arg0-11982 arg1-11983 (cdr local12090)))
                            (values '#f (car arg0-11982)))
                          (loop927 (cdr arg0-11982) arg1-11983 '#f))))))
                (values '#f (car arg0-11982)))
              (let ((local12134 (cdr arg0-11982)))
                (if (null? local12134)
                  (if (null? arg1-11983)
                    (values '#f '#f)
                    (values (car arg1-11983) '#f))
                  (if (if (pair? arg1-11983)
                        (eq? (car arg1-11983) (car local12134))
                        '#f)
                    (loop927 (cdr local12134) (cdr arg1-11983) '#f)
                    (loop927 (cdr local12134) arg1-11983 '#f))))))))
      arg1-11978
      (_keyword-procedure-required11328 arg0-11977)
      (_keyword-procedure-allowed11330 arg0-11977))))
  (define-values
   (_keyword-procedure-extract/method11355)
   (lambda (arg0-12159 arg1-12160 arg2-12161 arg3-12162)
     '#(keyword-procedure-extract/method
        "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
        746
        2
        36564
        3524
        #f)
     '(captures: #%modvars)
     (if (if (_keyword-procedure?11325 arg2-12161)
           (if (procedure-arity-includes?
                (_keyword-procedure-proc11326 arg2-12161)
                arg1-12160)
             (let ((localv12167 ?) (localv12168 ?))
               (begin
                 (set!-values (localv12167 localv12168)
                   (_check-kw-args11354 arg2-12161 arg0-12159))
                 (if localv12167 '#f (not localv12168))))
             '#f)
           '#f)
       (_keyword-procedure-proc11326 arg2-12161)
       (let ((local12173
              (if (_keyword-procedure?11325 arg2-12161)
                '#f
                (if (procedure? arg2-12161)
                  (let ((local12176 (procedure-extract-target arg2-12161)))
                    (if local12176
                      local12176
                      (begin
                        local12176
                        (if (_new-procedure?11350 arg2-12161) 'method '#f))))
                  '#f))))
         (if local12173
           (if (eq? local12173 'method)
             (begin
               local12173
               (let ((local12181
                      (_keyword-procedure-extract/method11355
                       arg0-12159
                       (add1 arg1-12160)
                       (_new-procedure-ref11351 arg2-12161)
                       (add1 arg3-12162))))
                 (lambda (arg0-12189 arg1-12190 . rest12191)
                   '#(...me/private/kw.ss:764:14
                      "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
                      764
                      14
                      37398
                      82
                      #t)
                   '(captures: local12181 arg2-12161)
                   (apply
                    local12181
                    arg0-12189
                    arg1-12190
                    (cons arg2-12161 rest12191)))))
             (_keyword-procedure-extract/method11355
              arg0-12159
              arg1-12160
              local12173
              arg3-12162))
           (lambda (arg0-12202 arg1-12203 . rest12204)
             '#(...me/private/kw.ss:769:10
                "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
                769
                10
                37609
                2475
                #t)
             '(captures: arg1-12160 arg2-12161 arg3-12162 #%modvars)
             (let ((localv12205 (box ?)) (localv12206 (box ?)))
               (begin
                 (set-boxes!
                  (localv12205 localv12206)
                  (if (_keyword-procedure?11325 arg2-12161)
                    (_check-kw-args11354 arg2-12161 arg0-12202)
                    (values '#f (car arg0-12202))))
                 (let ((local12213
                        (let ((local12214
                               (+
                                arg3-12162
                                (if (if (_keyword-method?11334 arg2-12161)
                                      '#t
                                      (_okm?11339 arg2-12161))
                                  '1
                                  '0))))
                          (if (>= arg1-12160 local12214)
                            (- arg1-12160 local12214)
                            arg1-12160))))
                   (let ((local12223
                          (if (if (null? rest12204) (null? arg0-12202) '#f)
                            (begin
                              rest12204
                              arg1-12203
                              arg0-12202
                              '"no arguments supplied")
                            (let ((with-handlers-handler212226
                                   (lambda (arg0-12227)
                                     '#(with-handlers-handler2
                                        "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
                                        784
                                        27
                                        38304
                                        407
                                        #f)
                                     (regexp-replace
                                      '#rx"^.*? given: x; (other )?"
                                      (exn-message arg0-12227)
                                      '""))))
                              (let ((local12232
                                     (continuation-mark-set-first
                                      '#f
                                      |_break-enabled-key@(quote #%paramz)|)))
                                (_call-handled-body11322
                                 local12232
                                 (begin0
                                   (lambda (arg0-12238)
                                     '(captures:
                                       local12232
                                       with-handlers-handler212226
                                       #%modvars)
                                     (_select-handler/no-breaks11319
                                      arg0-12238
                                      local12232
                                      (list
                                       (cons
                                        exn:fail?
                                        with-handlers-handler212226))))
                                   local12232
                                   with-handlers-handler212226)
                                 (begin0
                                   (lambda ()
                                     '(captures:
                                       arg0-12202
                                       arg1-12203
                                       rest12204)
                                     (apply
                                      raise-type-error
                                      'x
                                      '"x"
                                      '0
                                      'x
                                      (append
                                       rest12204
                                       (apply
                                        append
                                        (map list arg0-12202 arg1-12203)))))
                                   arg0-12202
                                   arg1-12203
                                   rest12204)))))))
                     (raise
                      (make-exn:fail:contract
                       (if (unbox localv12206)
                         (begin
                           localv12205
                           local12213
                           (if (_keyword-procedure?11325 arg2-12161)
                             (format
                              (string-append
                               '"procedure application: procedure: ~e;"
                               '" does not expect an argument with keyword ~a; ~a")
                              arg2-12161
                              (unbox localv12206)
                              local12223)
                             (begin
                               localv12206
                               (format
                                (string-append
                                 '"procedure application: expected a procedure that"
                                 '" accepts keyword arguments, given ~e; ~a")
                                arg2-12161
                                local12223))))
                         (if (unbox localv12205)
                           (begin
                             local12213
                             (format
                              (string-append
                               '"procedure application: procedure: ~e; requires"
                               '" an argument with keyword ~a, not supplied; ~a")
                              arg2-12161
                              (unbox localv12205)
                              local12223))
                           (begin
                             localv12205
                             (format
                              (string-append
                               '"procedure application: no case matching ~a non-keyword"
                               '" argument~a for: ~e; ~a")
                              (- local12213 '2)
                              (if (= '1 (- local12213 '2)) '"" '"s")
                              arg2-12161
                              local12223))))
                       (current-continuation-marks)))))))))))))
  (define-values
   (_check-inspector11356)
   (lambda (arg0-12292 arg1-12293)
     '#(check-inspector
        "/Users/jay/Dev/svn/plt/collects/scheme/private/define-struct.ss"
        52
        2
        1892
        149
        #f)
     (begin
       (if arg1-12293
         (if (inspector? arg1-12293)
           (void)
           (raise-type-error arg0-12292 '"inspector or #f" arg1-12293))
         (void))
       arg1-12293)))
  (define-values
   (_print-values11357)
   (lambda rest12298
     '#(print-values
        "/Users/jay/Dev/svn/plt/collects/scheme/private/modbeg.ss"
        10
        4
        214
        41
        #f)
     (for-each (current-print) rest12298)))
  (define-values
   (_for-each211358)
   (let ((for-each12301
          (case-lambda
           ((arg0-12302 arg1-12303)
            '#(...e/private/map.ss:45:12
               "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
               45
               12
               1472
               327
               #t)
            (if (if (procedure? arg0-12302)
                  (if (procedure-arity-includes? arg0-12302 '1)
                    (list? arg1-12303)
                    '#f)
                  '#f)
              (if (null? arg1-12303)
                (void)
                (begin
                  (arg0-12302 (car arg1-12303))
                  (let ((local12311 (cdr arg1-12303)))
                    (if (null? local12311)
                      (void)
                      (begin
                        (arg0-12302 (car local12311))
                        (let ((local12316 (cdr local12311)))
                          (if (null? local12316)
                            (void)
                            (begin
                              (arg0-12302 (car local12316))
                              (let ((local12321 (cdr local12316)))
                                (if (null? local12321)
                                  (void)
                                  (begin
                                    (arg0-12302 (car local12321))
                                    ((lambda (arg0-12328 arg1-12329)
                                       '#(loop
                                          "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
                                          49
                                          17
                                          1620
                                          145
                                          #f)
                                       (if (null? arg1-12329)
                                         (void)
                                         (begin
                                           (arg0-12328 (car arg1-12329))
                                           (let ((local12333 (cdr arg1-12329)))
                                             (if (null? local12333)
                                               (void)
                                               (begin
                                                 (arg0-12328 (car local12333))
                                                 (let ((local12338
                                                        (cdr local12333)))
                                                   (if (null? local12338)
                                                     (void)
                                                     (begin
                                                       (arg0-12328
                                                        (car local12338))
                                                       (let ((local12343
                                                              (cdr
                                                               local12338)))
                                                         (if (null? local12343)
                                                           (void)
                                                           (begin
                                                             (arg0-12328
                                                              (car local12343))
                                                             (let ((local12348
                                                                    (cdr
                                                                     local12343)))
                                                               (if (null?
                                                                    local12348)
                                                                 (void)
                                                                 (begin
                                                                   (arg0-12328
                                                                    (car
                                                                     local12348))
                                                                   (let ((local12353
                                                                          (cdr
                                                                           local12348)))
                                                                     (if (null?
                                                                          local12353)
                                                                       (void)
                                                                       (begin
                                                                         (arg0-12328
                                                                          (car
                                                                           local12353))
                                                                         (let ((local12358
                                                                                (cdr
                                                                                 local12353)))
                                                                           (if (null?
                                                                                local12358)
                                                                             (void)
                                                                             (begin
                                                                               (arg0-12328
                                                                                (car
                                                                                 local12358))
                                                                               (let ((local12363
                                                                                      (cdr
                                                                                       local12358)))
                                                                                 (if (null?
                                                                                      local12363)
                                                                                   (void)
                                                                                   (begin
                                                                                     (arg0-12328
                                                                                      (car
                                                                                       local12363))
                                                                                     (loop1285
                                                                                      arg0-12328
                                                                                      (cdr
                                                                                       local12363))))))))))))))))))))))))))
                                     arg0-12302
                                     (cdr local12321)))))))))))))
              (for-each arg0-12302 arg1-12303)))
           ((arg0-12374 arg1-12375 arg2-12376)
            '#(...e/private/map.ss:54:12
               "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
               54
               12
               1812
               483
               #t)
            (if (if (procedure? arg0-12374)
                  (if (procedure-arity-includes? arg0-12374 '2)
                    (if (list? arg1-12375)
                      (if (list? arg2-12376)
                        (= (length arg1-12375) (length arg2-12376))
                        '#f)
                      '#f)
                    '#f)
                  '#f)
              (if (null? arg1-12375)
                (void)
                (begin
                  (arg0-12374 (car arg1-12375) (car arg2-12376))
                  (let ((local12391 (cdr arg1-12375)))
                    (let ((local12393 (cdr arg2-12376)))
                      (if (null? local12391)
                        (void)
                        (begin
                          (arg0-12374 (car local12391) (car local12393))
                          (let ((local12400 (cdr local12391)))
                            (let ((local12402 (cdr local12393)))
                              (if (null? local12400)
                                (void)
                                (begin
                                  (arg0-12374
                                   (car local12400)
                                   (car local12402))
                                  ((lambda (arg0-12412 arg1-12413 arg2-12414)
                                     '#(loop
                                        "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
                                        60
                                        17
                                        2048
                                        209
                                        #f)
                                     (if (null? arg1-12413)
                                       (void)
                                       (begin
                                         (arg0-12412
                                          (car arg1-12413)
                                          (car arg2-12414))
                                         (let ((local12420 (cdr arg1-12413)))
                                           (let ((local12422 (cdr arg2-12414)))
                                             (if (null? local12420)
                                               (void)
                                               (begin
                                                 (arg0-12412
                                                  (car local12420)
                                                  (car local12422))
                                                 (let ((local12429
                                                        (cdr local12420)))
                                                   (let ((local12431
                                                          (cdr local12422)))
                                                     (if (null? local12429)
                                                       (void)
                                                       (begin
                                                         (arg0-12412
                                                          (car local12429)
                                                          (car local12431))
                                                         (let ((local12438
                                                                (cdr
                                                                 local12429)))
                                                           (let ((local12440
                                                                  (cdr
                                                                   local12431)))
                                                             (if (null?
                                                                  local12438)
                                                               (void)
                                                               (begin
                                                                 (arg0-12412
                                                                  (car
                                                                   local12438)
                                                                  (car
                                                                   local12440))
                                                                 (let ((local12447
                                                                        (cdr
                                                                         local12438)))
                                                                   (let ((local12449
                                                                          (cdr
                                                                           local12440)))
                                                                     (if (null?
                                                                          local12447)
                                                                       (void)
                                                                       (begin
                                                                         (arg0-12412
                                                                          (car
                                                                           local12447)
                                                                          (car
                                                                           local12449))
                                                                         (let ((local12456
                                                                                (cdr
                                                                                 local12447)))
                                                                           (let ((local12458
                                                                                  (cdr
                                                                                   local12449)))
                                                                             (if (null?
                                                                                  local12456)
                                                                               (void)
                                                                               (begin
                                                                                 (arg0-12412
                                                                                  (car
                                                                                   local12456)
                                                                                  (car
                                                                                   local12458))
                                                                                 (loop1286
                                                                                  arg0-12412
                                                                                  (cdr
                                                                                   local12456)
                                                                                  (cdr
                                                                                   local12458)))))))))))))))))))))))))
                                   arg0-12374
                                   (cdr local12400)
                                   (cdr local12402))))))))))))
              (for-each arg0-12374 arg1-12375 arg2-12376)))
           ((arg0-12475 . rest12476)
            '#(...e/private/map.ss:66:12
               "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
               66
               12
               2308
               36
               #t)
            (apply for-each arg0-12475 rest12476)))))
     for-each12301))
  (define-values
   (_new-apply11359)
   (_*make-keyword-procedure11352
    (lambda (arg0-12482 arg1-12483 arg2-12484 arg3-12485 . rest12486)
      '#(...vate/pre-base.ss:31:5
         "/Users/jay/Dev/svn/plt/collects/scheme/private/pre-base.ss"
         31
         5
         811
         103
         #t)
      '(captures: #%modvars)
      (_keyword-apply11353
       arg2-12484
       arg0-12482
       arg1-12483
       (apply list* arg3-12485 rest12486)))
    apply))
  (define-values (_insp11360) (current-inspector))
  (define-values
   (_struct:class11361
    _make-class11362
    _class?11363
    _class-name11364
    _class-pos11365
    _class-supers11366
    _class-self-interface11367
    _class-insp-mk11368
    _class-method-width11369
    _class-method-ht11370
    _class-method-ids11371
    _class-methods11372
    _class-beta-methods11373
    _class-meth-flags11374
    _class-field-width11375
    _class-field-ht11376
    _class-field-ids11377
    _class-struct:object11378
    _class-object?11379
    _class-make-object11380
    _class-field-ref11381
    _class-field-set!11382
    _class-init-args11383
    _class-init-mode11384
    _class-init11385
    _class-serializer11386
    _class-fixup11387
    _class-no-super-init?11388
    _set-class-struct:object!11389
    _set-class-object?!11390
    _set-class-make-object!11391
    _set-class-field-ref!11392
    _set-class-field-set!!11393
    _set-class-init!11394
    _set-class-serializer!11395
    _set-class-fixup!11396)
   (let ((localv12494 ?)
         (localv12495 ?)
         (localv12496 ?)
         (localv12497 ?)
         (localv12498 ?))
     (begin
       (set!-values (localv12494
                     localv12495
                     localv12496
                     localv12497
                     localv12498)
         (make-struct-type
          'class
          '#f
          '25
          '0
          '#f
          '()
          (_check-inspector11356 'define-struct _insp11360)
          '#f
          '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 19 20 24)
          '#f))
       (values
        localv12494
        localv12495
        localv12496
        (make-struct-field-accessor localv12497 '0 'name)
        (make-struct-field-accessor localv12497 '1 'pos)
        (make-struct-field-accessor localv12497 '2 'supers)
        (make-struct-field-accessor localv12497 '3 'self-interface)
        (make-struct-field-accessor localv12497 '4 'insp-mk)
        (make-struct-field-accessor localv12497 '5 'method-width)
        (make-struct-field-accessor localv12497 '6 'method-ht)
        (make-struct-field-accessor localv12497 '7 'method-ids)
        (make-struct-field-accessor localv12497 '8 'methods)
        (make-struct-field-accessor localv12497 '9 'beta-methods)
        (make-struct-field-accessor localv12497 '10 'meth-flags)
        (make-struct-field-accessor localv12497 '11 'field-width)
        (make-struct-field-accessor localv12497 '12 'field-ht)
        (make-struct-field-accessor localv12497 '13 'field-ids)
        (make-struct-field-accessor localv12497 '14 'struct:object)
        (make-struct-field-accessor localv12497 '15 'object?)
        (make-struct-field-accessor localv12497 '16 'make-object)
        (make-struct-field-accessor localv12497 '17 'field-ref)
        (make-struct-field-accessor localv12497 '18 'field-set!)
        (make-struct-field-accessor localv12497 '19 'init-args)
        (make-struct-field-accessor localv12497 '20 'init-mode)
        (make-struct-field-accessor localv12497 '21 'init)
        (make-struct-field-accessor localv12497 '22 'serializer)
        (make-struct-field-accessor localv12497 '23 'fixup)
        (make-struct-field-accessor localv12497 '24 'no-super-init?)
        (make-struct-field-mutator localv12498 '14 'struct:object)
        (make-struct-field-mutator localv12498 '15 'object?)
        (make-struct-field-mutator localv12498 '16 'make-object)
        (make-struct-field-mutator localv12498 '17 'field-ref)
        (make-struct-field-mutator localv12498 '18 'field-set!)
        (make-struct-field-mutator localv12498 '21 'init)
        (make-struct-field-mutator localv12498 '22 'serializer)
        (make-struct-field-mutator localv12498 '23 'fixup)))))
  (define-values
   (_prop:object11405 _object?11407 _object-ref11404)
   (make-struct-type-property 'object))
  (define-values
   (_struct:interface11402
    _make-interface11403
    _interface?11399
    _interface-name11400
    _interface-supers11408
    _interface-all-implemented11409
    _interface-public-ids11401
    _interface-class11410
    _interface-properties11406
    _set-interface-all-implemented!11411
    _set-interface-class!11412)
   (let ((localv12647 ?)
         (localv12648 ?)
         (localv12649 ?)
         (localv12650 ?)
         (localv12651 ?))
     (begin
       (set!-values (localv12647
                     localv12648
                     localv12649
                     localv12650
                     localv12651)
         (make-struct-type
          'interface
          '#f
          '6
          '0
          '#f
          '()
          (_check-inspector11356 'define-struct _insp11360)
          '#f
          '(0 1 3 5)
          '#f))
       (values
        localv12647
        localv12648
        localv12649
        (make-struct-field-accessor localv12650 '0 'name)
        (make-struct-field-accessor localv12650 '1 'supers)
        (make-struct-field-accessor localv12650 '2 'all-implemented)
        (make-struct-field-accessor localv12650 '3 'public-ids)
        (make-struct-field-accessor localv12650 '4 'class)
        (make-struct-field-accessor localv12650 '5 'properties)
        (make-struct-field-mutator localv12651 '2 'all-implemented)
        (make-struct-field-mutator localv12651 '4 'class)))))
  (define-values
   (_object<%>11413)
   ((let ((local12705 _struct:interface11402))
      (let ((localv12706 ?)
            (localv12707 ?)
            (localv12708 ?)
            (localv12709 ?)
            (localv12710 ?))
        (begin
          (set!-values (localv12706
                        localv12707
                        localv12708
                        localv12709
                        localv12710)
            (make-struct-type
             'interface:object%
             local12705
             '0
             '0
             '#f
             '()
             _insp11360))
          localv12707)))
    'object%
    '()
    '#f
    '()
    '#f
    '()))
  (call-with-values
   (lambda ()
     (let ((local12718 _object<%>11413))
       (let ((local12719 (make-hasheq)))
         (begin
           (hash-set! local12719 local12718 '#t)
           (_for-each211358
            (lambda (arg0-12725)
              '#(...lass-internal.ss:2510:16
                 "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
                 2510
                 16
                 102257
                 116
                 #t)
              '(captures: local12719 #%modvars)
              (hash-for-each
               (_interface-all-implemented11409 arg0-12725)
               (lambda (arg0-12729 arg1-12730)
                 '#(...lass-internal.ss:2513:19
                    "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
                    2513
                    19
                    102330
                    41
                    #t)
                 '(captures: local12719)
                 (hash-set! local12719 arg0-12729 '#t))))
            (_interface-supers11408 local12718))
           (_set-interface-all-implemented!11411 local12718 local12719)))))
   _print-values11357)
  (define-values
   (_object%11398)
   ((let ((local12762 _struct:class11361))
      (let ((localv12763 ?)
            (localv12764 ?)
            (localv12765 ?)
            (localv12766 ?)
            (localv12767 ?))
        (begin
          (set!-values (localv12763
                        localv12764
                        localv12765
                        localv12766
                        localv12767)
            (make-struct-type
             'class:object%
             local12762
             '0
             '0
             '#f
             '()
             _insp11360))
          localv12764)))
    'object%
    '0
    (vector '#f)
    _object<%>11413
    void
    '0
    (make-hasheq)
    '()
    (vector)
    (vector)
    (vector)
    '0
    (make-hasheq)
    '()
    'struct:object
    _object?11407
    'make-object
    'field-ref-not-needed
    'field-set!-not-needed
    '()
    'normal
    (lambda (arg0-12776 arg1-12777 arg2-12778 arg3-12779 arg4-12780 arg5-12781)
      '#(...lass-internal.ss:2566:19
         "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
         2566
         19
         103783
         143
         #t)
      '(captures: #%modvars)
      (begin
        arg4-12780
        arg3-12779
        arg2-12778
        arg1-12777
        (begin
          (if (null? arg5-12781)
            (void)
            (_unused-args-error11414 arg0-12776 arg5-12781))
          (void))))
    (lambda (arg0-12785)
      '#(...lass-internal.ss:2571:19
         "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
         2571
         19
         103933
         20
         #t)
      '#(()))
    (lambda (arg0-12786 arg1-12787)
      '#(...lass-internal.ss:2572:19
         "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
         2572
         19
         103978
         26
         #t)
      (void))
    '#t))
  (call-with-values
   (lambda ()
     (vector-set! (_class-supers11366 _object%11398) '0 _object%11398))
   _print-values11357)
  (call-with-values
   (lambda ()
     (let ((localv12792 ?)
           (localv12793 ?)
           (localv12794 ?)
           (localv12795 ?)
           (localv12796 ?))
       (begin
         (set!-values (localv12792
                       localv12793
                       localv12794
                       localv12795
                       localv12796)
           (make-struct-type
            'object
            '#f
            '0
            '0
            '#f
            (list (cons _prop:object11405 _object%11398))
            '#f))
         (begin
           localv12796
           localv12795
           localv12794
           (begin
             (_set-class-struct:object!11389 _object%11398 localv12792)
             (_set-class-make-object!11391 _object%11398 localv12793))))))
   _print-values11357)
  (call-with-values
   (lambda () (_set-class-object?!11390 _object%11398 _object?11407))
   _print-values11357)
  (call-with-values
   (lambda () (_set-interface-class!11412 _object<%>11413 _object%11398))
   _print-values11357)
  (define-values
   (_make-named-arg-string11415)
   (lambda (arg0-12815)
     '#(make-named-arg-string
        "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
        2827
        2
        113751
        287
        #f)
     ((lambda (arg0-12818 arg1-12819)
        '#(loop
           "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
           2828
           4
           113792
           245
           #f)
        (if (null? arg0-12818)
          '""
          (if (= arg1-12819 '3)
            '" ..."
            (let ((local12823
                   (let ((local12824 (cdr arg0-12818)))
                     (let ((local12826 (add1 arg1-12819)))
                       (if (null? local12824)
                         '""
                         (if (= local12826 '3)
                           '" ..."
                           (let ((local12831
                                  (let ((local12832 (cdr local12824)))
                                    (let ((local12834 (add1 local12826)))
                                      (if (null? local12832)
                                        '""
                                        (if (= local12834 '3)
                                          '" ..."
                                          (let ((local12839
                                                 (loop3414
                                                  (cdr local12832)
                                                  (add1 local12834))))
                                            (format
                                             '" (~a ~e)~a"
                                             (caar local12832)
                                             (cdar local12832)
                                             local12839))))))))
                             (format
                              '" (~a ~e)~a"
                              (caar local12824)
                              (cdar local12824)
                              local12831))))))))
              (format
               '" (~a ~e)~a"
               (caar arg0-12818)
               (cdar arg0-12818)
               local12823)))))
      arg0-12815
      '0)))
  (define-values
   (_unused-args-error11414)
   (lambda (arg0-12862 arg1-12863)
     '#(unused-args-error
        "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
        2838
        2
        114042
        244
        #f)
     '(captures: #%modvars)
     (let ((local12864 (_make-named-arg-string11415 arg1-12863)))
       (_obj-error11397
        'instantiate
        '"unused initialization arguments:~a~a"
        local12864
        (_for-class/which11416
         '"instantiated"
         (_class-name11364 (_object-ref11404 arg0-12862)))))))
  (define-values
   (_struct:exn:fail:object11417
    _make-exn:fail:object11418
    _exn:fail:object?11419)
   (let ((localv12874 ?)
         (localv12875 ?)
         (localv12876 ?)
         (localv12877 ?)
         (localv12878 ?))
     (begin
       (set!-values (localv12874
                     localv12875
                     localv12876
                     localv12877
                     localv12878)
         (make-struct-type
          'exn:fail:object
          struct:exn:fail
          '0
          '0
          '#f
          '()
          (_check-inspector11356 'define-struct _insp11360)
          '#f
          '()
          '#f))
       (values localv12874 localv12875 localv12876))))
  (define-values
   (_obj-error11397)
   (lambda (arg0-12894 . rest12895)
     '#(obj-error
        "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
        3681
        2
        147456
        177
        #f)
     '(captures: #%modvars)
     (raise
      (_make-exn:fail:object11418
       (string-append
        (format '"~a: " arg0-12894)
        (_new-apply11359 format rest12895))
       (current-continuation-marks)))))
  (define-values
   (_for-class/which11416)
   (lambda (arg0-12905 arg1-12906)
     '#(for-class/which
        "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
        3688
        2
        147713
        94
        #f)
     (if arg1-12906 (format '" for ~a class: ~a" arg0-12905 arg1-12906) '"")))
  (call-with-values
   (lambda ()
     (if (= '4 (|_ctype-sizeof@(quote #%foreign)| |__float@(quote #%foreign)|))
       (void)
       (error
        'foreign
        '"internal error: float has a bad size (~s)"
        (|_ctype-sizeof@(quote #%foreign)| |__float@(quote #%foreign)|))))
   _print-values11357)
  (call-with-values
   (lambda ()
     (if (=
          '8
          (|_ctype-sizeof@(quote #%foreign)| |__double*@(quote #%foreign)|))
       (void)
       (error
        'foreign
        '"internal error: double has a bad size (~s)"
        (|_ctype-sizeof@(quote #%foreign)| |__double*@(quote #%foreign)|))))
   _print-values11357))
