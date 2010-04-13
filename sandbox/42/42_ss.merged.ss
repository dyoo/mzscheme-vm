(begin
  (module 42_ss.merged ....
    (define-values
     (_print-values1736)
     (#%closed
      print-values1454
      (lambda rest1737
        '#(print-values
           #<path:/home/dyoo/local/plt/collects/scheme/private/modbeg.ss>
           10
           4
           214
           72
           #f)
        (begin (for-each (current-print) rest1737) (apply values rest1737)))))
    (#%apply-values (#%checked _print-values1736) '42)))
