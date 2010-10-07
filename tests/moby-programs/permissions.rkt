#lang s-exp "../../src/lang/base.rkt"

;; Any program that's compiled with require-permission should have
;; the permissions of the module as part of the module record.

(printf "permissions.rkt\n")

(require "../../src/lang/permissions/require-permission.rkt")

(require-permission "network")