(begin
  (module square_ss.merged ....
    (define-values
     (_print-values1737)
     (#%closed
      print-values1455
      (lambda rest1738
        '#(print-values
           #<path:/home/dyoo/local/plt/collects/scheme/private/modbeg.ss>
           10
           4
           214
           72
           #f)
        (begin (for-each (current-print) rest1738) (apply values rest1738)))))
    (#%apply-values (#%checked _print-values1737) '9)
    (#%apply-values (#%checked _print-values1737) '16)))
