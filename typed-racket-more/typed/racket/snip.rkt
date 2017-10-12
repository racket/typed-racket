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

(begin-for-syntax
  (module* #%contract-defs-names #f (#%plain-module-begin)))
(module* #%contract-defs #f (#%plain-module-begin))

(type-environment
 [snip% (parse-type #'Snip%)]
 [snip-admin% (parse-type #'Snip-Admin%)]
 [snip-class% (parse-type #'Snip-Class%)]
 [string-snip% (parse-type #'String-Snip%)]
 [style-delta% (parse-type #'Style-Delta%)]
 [style-list% (parse-type #'Style-List%)])
