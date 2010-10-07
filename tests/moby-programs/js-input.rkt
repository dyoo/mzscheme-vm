#lang s-exp "../../src/lang/base.rkt"

(require "../../src/jsworld/jsworld.rkt")


(define (make-ingredient-checkbox-sexp ingredient)
  (local [(define (on-check w v)
                   (cond
                     [v
                      (cons ingredient w)]
                     [else
                      (remove ingredient w)]))]
    (list (js-div)
          (list (js-text ingredient))
          (list (js-input "checkbox"
                          on-check
                          `(("value" ,ingredient)))))))

(define c1 (make-ingredient-checkbox-sexp "mushrooms"))
(define c2 (make-ingredient-checkbox-sexp "green peppers"))
(define c3 (make-ingredient-checkbox-sexp "olives"))

(define (draw w)
  (list (js-div)
       c1
       c2
       c3
       (list (js-text (format "The world is: ~s" w)))))

(define (draw-css w)
  '())


(js-big-bang '()
             (on-draw draw draw-css))