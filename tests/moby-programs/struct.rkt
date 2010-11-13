#lang s-exp "../../lang/wescheme.rkt"
"struct.rkt"

(let-values ([(a-struct-type
              constructor
              predicate
              accessor
              mutator)
             (make-struct-type 'pair 
                               #f
                               2
                               0)])
  (check-expect (struct-type? a-struct-type) true)
  (check-expect (struct-type? (constructor 3 4)) false)
  (check-expect (predicate (constructor 3 4)) true)
  (check-expect (predicate (cons 3 4)) false)

  (let ([f (make-struct-field-accessor accessor 0)]
        [r (make-struct-field-accessor accessor 1)]
        [set-f! (make-struct-field-mutator mutator 0)]
        [set-r! (make-struct-field-mutator mutator 1)])
    (let ([p1 (constructor 17 'foo)])
      (check-expect (f p1) 17)
      (check-expect (r p1) 'foo)

      (set-f! p1 'something-else)
      (check-expect (f p1) 'something-else)
      (set-r! p1 1024)
      (check-expect (r p1) '1024))))

"struct.rkt end"