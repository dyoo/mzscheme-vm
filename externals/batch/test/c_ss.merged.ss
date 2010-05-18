(module c_ss.merged '#%kernel
  (define-values
   (_print-values1348)
   (lambda rest1352
     '#(print-values
        "/Users/jay/Dev/svn/plt/collects/scheme/private/modbeg.ss"
        10
        4
        214
        72
        #f)
     (begin (for-each (current-print) rest1352) (apply values rest1352))))
  (define-values (_a1349) '5)
  (define-values (_b1350) '4)
  (define-values (_c1351) (add1 _a1349 _b1350))
  (call-with-values (lambda () _c1351) _print-values1348))
