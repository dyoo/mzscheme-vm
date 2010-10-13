#lang s-exp "../../lang/base.rkt"

;; Any program that's compiled with require-permission should have
;; the permissions of the module as part of the module record.

(printf "permissions.rkt\n")

(require "../../lang/permissions/require-permission.rkt")

(require-permission "network")