#lang s-exp typed-racket/base-env/extra-env-lang

;; This module provides a base type environment including
;; racket/snip bindings

(require racket/snip/private/snip
         racket/snip/private/snip-admin
         racket/snip/private/style
         "private/gui-types.rkt"
         (for-syntax (submod "private/gui-types.rkt" #%type-decl)))

(provide Snip%
         Snip-Admin%
         Snip-Class%
         String-Snip%
         Style<%>
         Style-Delta%
         Style-List%)

(require "generate-predef-con.rkt"
         typed-racket/base-env/base-types-extra)

(type-environment
 [snip% (parse-type #'Snip%)]
 [snip-admin% (parse-type #'Snip-Admin%)]
 [snip-class% (parse-type #'Snip-Class%)]
 [string-snip% (parse-type #'String-Snip%)]
 [style-delta% (parse-type #'Style-Delta%)]
 [style-list% (parse-type #'Style-List%)])
