#lang racket/base
(require racket/list
	 "../main.rkt"
	 "check-coverage.rkt")

(unless (empty? (untouched-wescheme-primitives))
  (print-coverage-report)
  (printf "Press Enter to continue.\n")
  (void (read-line)))

(run-in-browser "all-tests.rkt")
