#lang s-exp "../../lang/wescheme.rkt"
"sierpinski"
(let sierpinski ([n 6])
  (if (zero? n)
      (triangle 10 'solid 'red)
      (let ([next (sierpinski (- n 1))])
        (above next (beside next next)))))